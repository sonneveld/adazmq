--  Multithreaded Hello World server

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure MTServer is

   use type GNAT.Formatted_String.Formatted_String;

   type Context_Access is access all ZMQ.Context_Type'Class;

   task type Worker_Routine_Type is
      entry Start (Context_A : Context_Access);
   end Worker_Routine_Type;

   task body Worker_Routine_Type is
      Context : Context_Access := null;
   begin
      accept Start (Context_A : Context_Access) do
         Context := Context_A;
      end Start;

      declare
         --  Socket to talk to dispatcher
         Receiver : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REP);
      begin
         Receiver.Connect ("inproc://workers");
         loop
            declare
               Msg : constant String := Receiver.Recv;
            begin
               Ada.Text_IO.Put_Line (-(+"Received request: [%s]"&Msg));
               --  Do some 'work'
               delay 1.0;
               --  Send reply back to client
               Receiver.Send ("World");
            end;
         end loop;
         --  We never get here, but clean up anyhow
         Receiver.Close;
      end;
   end Worker_Routine_Type;

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : aliased ZMQ.Context_Type := ZMQ.New_Context;
      --  Socket to talk to clients
      Clients : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
      --  Socket to talk to workers
      Workers : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_DEALER);
   begin
      Clients.Bind ("tcp://*:5555");
      Workers.Bind ("inproc://workers");

      --  Launch pool of worker threads
      declare
         Tasks : array (1 .. 5) of Worker_Routine_Type;
      begin
         for T of Tasks loop
            T.Start (Context'Unrestricted_Access);
         end loop;

         --  Connect work threads to client threads via a queue proxy
         ZMQ.Proxy (Clients, Workers);

         --  We never get here, but clean up anyhow
         Clients.Close;
         Workers.Close;

         --  Program will not leave this block until tasks above exit.
      end;

      Context.Term;

      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end MTServer;
