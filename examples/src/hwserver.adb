with Ada.Command_Line;
with Ada.Text_IO;

with ZMQ;

procedure HWServer is

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : constant ZMQ.Context_Type := ZMQ.New_Context;
      Responder : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REP);
   begin
      Responder.Bind ("tcp://*:5555");

      loop
         declare
            Dummy : String := Responder.Recv;
         begin
            Ada.Text_IO.Put_Line ("Received hello");
            delay 1.0;
            Responder.Send ("World");
         end;
      end loop;

      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end HWServer;


