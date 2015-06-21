--  Pubsub envelope subscriber

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

use type GNAT.Formatted_String.Formatted_String;

procedure PSEnvSub is

   function Main return Ada.Command_Line.Exit_Status
   is
      --  Prepare our context and subscriber
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Subscriber : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
   begin
      Subscriber.Connect ("tcp://localhost:5563");
      Subscriber.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, "B");

      loop
         declare
            Address : constant String := Subscriber.Recv; --  Read envelope with address
            Contents : constant String := Subscriber.Recv; --  Read message contents
         begin
            Ada.Text_IO.Put_Line(-(+"[%s] %s"&Address&Contents));
         end;
      end loop;

      --  We never get here, but clean up anyhow
      Subscriber.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end PSEnvSub;
