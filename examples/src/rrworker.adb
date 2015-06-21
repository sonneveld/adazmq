--  Hello World worker
--  Connects REP socket to tcp://localhost:5560
--  Expects "Hello" from client, replies with "World"

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure RRWorker is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      --  Socket to talk to clients
      Responder : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REP);
   begin
      Responder.Connect ("tcp://localhost:5560");
      loop
         declare
            Msg : String := Responder.Recv;
         begin
            Ada.Text_IO.Put_Line (-(+("Received reply [%s]")&Msg));
         end;

         --  Do some 'work'
         delay 1.0;

         --  Send reply back to client
         Responder.Send ("World");
      end loop;

      --  We never get here, but clean up anyhow
      Responder.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end RRWorker;




