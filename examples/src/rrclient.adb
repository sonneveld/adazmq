--  Hello World client
--  Connects REQ socket to tcp://localhost:5559
--  Sends "Hello" to server, expects "World" back

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure RRClient is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      --  Socket to talk to server
      Requester : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
   begin
      Requester.Connect ("tcp://localhost:5559");
      for Request_Nbr in 1 .. 10 loop
         Requester.Send ("Hello");
         declare
            Msg : String := Requester.Recv;
         begin
            Ada.Text_IO.Put_Line (-(+("Received reply %d [%s]")&Request_Nbr&Msg));
         end;
      end loop;
      Requester.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end RRClient;
