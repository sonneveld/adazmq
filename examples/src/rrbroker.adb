--  Simple request-reply broker

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure RRBroker is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         --  Prepare our context and socket
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Frontend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
         Backend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_DEALER);
      begin
         Frontend.Bind ("tcp://*:5559");
         Backend.Bind ("tcp://*:5560");

         --  Switch messages between sockets
         loop
            declare
               Items : ZMQ.Poll_Item_Array_Type :=
                  (ZMQ.New_Poll_Item (Frontend, Poll_In => True),
                  ZMQ.New_Poll_Item (Backend, Poll_In => True));
            begin
               ZMQ.Poll (Items);

               --  Frontend
               if ZMQ.Is_Readable (Items (Items'First)) then
                  Frontend_Loop :
                  loop
                     --  Process all parts of the message
                     declare
                        Message : ZMQ.Message_Type := ZMQ.Create_Message;
                        More : Boolean;
                     begin
                        Message.Recv (Frontend);
                        More := Message.More;
                        Message.Send (Backend, Send_More => More);
                        exit Frontend_Loop when not More; --  Last message part
                     end;
                  end loop Frontend_Loop;
               end if;

               --  Backend
               if ZMQ.Is_Readable (Items (Items'First + 1)) then
                  Backend_Loop :
                  loop
                     --  Process all parts of the message
                     declare
                        Message : ZMQ.Message_Type := ZMQ.Create_Message;
                        More : Boolean;
                     begin
                        Message.Recv (Backend);
                        More := Message.More;
                        Message.Send (Frontend, Send_More => More);
                        exit Backend_Loop when not More; --  Last message part
                     end;
                  end loop Backend_Loop;
               end if;
            end;
         end loop;

         --  We never get here, but clean up anyhow
         Backend.Close;
         Frontend.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end RRBroker;
