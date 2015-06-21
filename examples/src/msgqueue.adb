--  Simple message queuing broker
--  Same as request-reply broker but using shared queue proxy

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure MsgQueue is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Socket facing clients
         Frontend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
         --  Socket facing services
         Backend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_DEALER);
      begin
         Frontend.Bind ("tcp://*:5559");
         Backend.Bind ("tcp://*:5560");

         --  Start the proxy
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
end MsgQueue;
