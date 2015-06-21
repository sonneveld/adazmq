with Ada.Command_Line;
with Ada.Text_IO;

with ZMQ;

procedure Version is

   function Main return Ada.Command_Line.Exit_Status
   is
      Major, Minor, Patch : Natural;
   begin
      ZMQ.Version (Major, Minor, Patch);
      Ada.Text_IO.Put_Line ("Current 0MQ version is "&Major'Img&"."&Minor'Img&"."&Patch'Img);
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end Version;
