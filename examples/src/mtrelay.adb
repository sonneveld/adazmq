--  Multithreaded relay

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure MTRelay is

   use type GNAT.Formatted_String.Formatted_String;

   type Context_Access is access all ZMQ.Context_Type'Class;


   task Step_1_Task is
      entry Start (Context_A : Context_Access);
   end Step_1_Task;

   task body Step_1_Task is
      Context : Context_Access := null;
   begin
      accept Start (Context_A : Context_Access) do
         Context := Context_A;
      end Start;

      declare
         --  Connect to step2 and tell it we're ready
         Xmitter : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PAIR);
      begin
         Xmitter.Connect ("inproc://step2");
         Ada.Text_IO.Put_Line ("Step 1 ready, signaling step 2");
         Xmitter.Send ("READY");
         Xmitter.Close;
      end;
   end Step_1_Task;


   task Step_2_Task is
      entry Start (Context_A : Context_Access);
   end Step_2_Task;

   task body Step_2_Task is
      Context : Context_Access := null;
   begin
      accept Start (Context_A : Context_Access) do
         Context := Context_A;
      end Start;

      declare
         --  Bind inproc socket before starting step1
         Receiver : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PAIR);
      begin
         Receiver.Bind ("inproc://step2");
         Step_1_Task.Start (Context);

         --  Wait for signal and pass it on
         declare
            Unused_Msg : String := Receiver.Recv;
         begin
            null;
         end;
         Receiver.Close;
      end;

      --  Connect to step3 and tell it we're ready
      declare
         Xmitter : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PAIR);
      begin
         Xmitter.Connect ("inproc://step3");
         Ada.Text_IO.Put_Line ("Step 2 ready, signaling step 3");
         Xmitter.Send ("READY");
         Xmitter.Close;
      end;
   end Step_2_Task;


   function Main return Ada.Command_Line.Exit_Status
   is
      Context : aliased ZMQ.Context_Type := ZMQ.New_Context;
   begin

      declare
         --  Bind inproc socket before starting step2
         Receiver : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PAIR);
      begin
         Receiver.Bind ("inproc://step3");
         Step_2_Task.Start (Context'Unrestricted_Access);

         --  Wait for signal
         declare
            Unused_Msg : String := Receiver.Recv;
         begin
            null;
         end;
         Receiver.Close;
      end;

      Ada.Text_IO.Put_Line ("Test successful!");
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end MTRelay;
