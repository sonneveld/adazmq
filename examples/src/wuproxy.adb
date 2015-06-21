--  Weather proxy device

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure WUPRoxy is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  This is where the weather server sits
         Frontend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_XSUB);
         --  This is our public endpoint for subscribers
         Backend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_XPUB);
      begin
         Frontend.Connect ("tcp://192.168.55.210:5556");
         Backend.Bind ("tcp://10.1.1.0:8100");

         --  Run the proxy until the user interrupts us
         ZMQ.Proxy (Frontend, Backend);

         --  We never get here...
         Backend.Close;
         Frontend.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end WUPRoxy;
