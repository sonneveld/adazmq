--  Reading from multiple sockets
--  This version uses zmq_poll()

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure MSPoller is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         --  Prepare our context and socket
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Connect to task ventilator
         Receiver : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PULL);
         --  Connect to weather server
         Subscriber : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
      begin
         Receiver.Connect ("tcp://localhost:5557");
         Subscriber.Connect ("tcp://localhost:5556");
         Subscriber.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, -(+"%05d "&Integer (10001)));

         --  Process messages from both sockets
         loop
            declare
               Items : ZMQ.Poll_Item_Array_Type :=
                  (ZMQ.New_Poll_Item (Receiver, Poll_In => True),
                  ZMQ.New_Poll_Item (Subscriber, Poll_In => True));
            begin
               ZMQ.Poll (Items);

               if ZMQ.Is_Readable (Items (Items'First)) then
                  declare
                     Msg : constant String := Receiver.Recv;
                  begin
                     --  Process task
                     Ada.Text_IO.Put_Line ("task: " & Msg);
                     null;
                  end;
               end if;

               if ZMQ.Is_Readable (Items (Items'First + 1)) then
                  declare
                     Msg : constant String := Subscriber.Recv;
                  begin
                     --  Process weather update
                     Ada.Text_IO.Put_Line ("wu: " & Msg);
                     null;
                  end;
               end if;
            end;

         end loop;

         Receiver.Close;
         Subscriber.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end MSPoller;
