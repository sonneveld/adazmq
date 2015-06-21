with Ada.Command_Line;
with Ada.Text_IO;

with ZMQ;

procedure HWClient is

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      Ada.Text_IO.Put_Line ("Connecting to hello world server...");

      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Requester : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      begin
         Requester.Connect ("tcp://localhost:5555");

         for Request_Nbr in 0 .. 10 loop
            Ada.Text_IO.Put_Line ("Sending Hello "&Request_Nbr'Img&"...");
            Requester.Send ("Hello");
            declare
               Dummy : String := Requester.Recv;
            begin
               Ada.Text_IO.Put_Line ("Received World " & Request_Nbr'Img);
            end;
         end loop;
         Requester.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end HWClient;
