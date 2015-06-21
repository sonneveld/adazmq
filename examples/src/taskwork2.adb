--  Task worker - design 2
--  Adds pub-sub flow to receive and respond to kill signal

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure TaskWork2 is

   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin

      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Socket to receive messages on
         Receiver : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PULL);
         --  Socket to send messages to
         Sender : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUSH);
         --  Socket for control input
         Controller : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
      begin
         Receiver.Connect ("tcp://localhost:5557");
         Sender.Connect ("tcp://localhost:5558");
         Controller.Connect ("tcp://localhost:5559");
         Controller.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, "");

         --  Process messages from either socket
         Process_Loop :
         loop

            declare
               Items : ZMQ.Poll_Item_Array_Type :=
                  (ZMQ.New_Poll_Item (Receiver, Poll_In => True),
                  ZMQ.New_Poll_Item (Controller, Poll_In => True));
            begin
               ZMQ.Poll (Items);

               if ZMQ.Is_Readable (Items (Items'First)) then

                  declare
                     Buf : constant String := Receiver.Recv;
                  begin
                     Ada.Text_IO.Put (Buf &".");  --  Show progress
                     Ada.Text_IO.Flush;

                     delay Duration'Value (Buf) / 1000.0; --  Do the work

                     Sender.Send ("");  --  Send results to sink
                  end;

               end if;

               --  Any waiting controller command acts as 'KILL'
               exit Process_Loop when ZMQ.Is_Readable (Items (Items'First + 1));

            end;
         end loop Process_Loop;

         Receiver.Close;
         Sender.Close;
         Controller.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end TaskWork2;
