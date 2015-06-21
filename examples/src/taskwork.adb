--  Task worker
--  Connects PULL socket to tcp://localhost:5557
--  Collects workloads from ventilator via that socket
--  Connects PUSH socket to tcp://localhost:5558
--  Sends results to sink via that socket

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure TaskWork is

   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin

      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Socket to receive messages on
         Receiver : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PULL);
         --  Socket to send messages to
         Sender : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUSH);
      begin
         Receiver.Connect ("tcp://localhost:5557");
         Sender.Connect ("tcp://localhost:5558");

         --  Process tasks forever
         loop
            declare
               Buf : constant String := Receiver.Recv;
            begin
               Ada.Text_IO.Put (Buf &".");  --  Show progress
               Ada.Text_IO.Flush;

               delay Duration'Value (Buf) / 1000.0; --  Do the work

               Sender.Send ("");
            end;
         end loop;

         Receiver.Close;
         Sender.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end TaskWork;
