--  Reading from multiple sockets
--  This version uses a simple recv loop

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure MSReader is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         --  Prepare our context and socket
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Connect to task ventilator
         Receiver : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PULL);
         --  Connect to weather server
         Subscriber : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
      begin
         Receiver.Connect ("tcp://localhost:5557");
         Subscriber.Connect ("tcp://localhost:5556");
         Subscriber.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, -(+"%05d "&Integer (10001)));

         --  Process messages from both sockets
         --  We prioritize traffic from the task ventilator
         loop

            Receiver_Loop :
            loop
               begin
                  declare
                     Msg : String := Receiver.Recv (Do_Not_Wait => True);
                  begin
                     --  Process task
                     Ada.Text_IO.Put_Line ("task");
                     null;
                  end;
               exception
                  when ZMQ.ZMQ_Error =>
                     -- EAGAIN
                     exit Receiver_Loop;
               end;
            end loop Receiver_Loop;

            Subscriber_Loop :
            loop
               begin
                  declare
                     Msg : String := Subscriber.Recv (Do_Not_Wait => True);
                  begin
                     --  Process weather update
                     Ada.Text_IO.Put_Line ("wu");
                     null;
                  end;
               exception
                  when ZMQ.ZMQ_Error =>
                     -- EAGAIN
                     exit Subscriber_Loop;
               end;
            end loop Subscriber_Loop;

            --  No activity, so sleep for 1 msec
            delay 0.001;
         end loop;

         Receiver.Close;
         Subscriber.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end MSReader;
