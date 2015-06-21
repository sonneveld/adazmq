--  Pubsub envelope publisher

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

use type GNAT.Formatted_String.Formatted_String;

procedure PSEnvPub is

   function Main return Ada.Command_Line.Exit_Status
   is
      --  Prepare our context and publisher
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Publisher : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUB);
   begin
      Publisher.Bind ("tcp://*:5563");

      loop
         --  Write two messages, each with an envelope and content
         Publisher.Send ("A", Send_More => True);
         Publisher.Send ("We don't want to see this");

         Publisher.Send ("B", Send_More => True);
         Publisher.Send ("We would like to see this");

         delay 1.0;
      end loop;

      --  We never get here, but clean up anyhow
      Publisher.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end PSEnvPub;
