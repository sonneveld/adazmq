--  Synchronized subscriber

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure SyncSub is

   use type GNAT.Formatted_String.Formatted_String;

   Subscribers_Expected : constant := 10; --  We wait for 10 subscribers

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  First, connect our subscriber socket
         Subscriber : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
         --  Second, synchronize with publisher
         Sync_Client : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      begin
         --  First, connect our subscriber socket
         Subscriber.Connect ("tcp://localhost:5561");
         Subscriber.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, "");

         --  0MQ is so fast, we need to wait a while...
         delay 1.0;

         --  Second, synchronize with publisher
         Sync_Client.Connect ("tcp://localhost:5562");

         --  - send a synchronization request
         Sync_Client.Send ("");

         --  - wait for synchronization reply
         declare
            Unused_Reply : String := Sync_Client.Recv;
         begin
            null;
         end;

         --  Third, get our updates and report how many we got
         declare
            Update_Nbr : Natural := 0;
         begin
            Recv_Loop :
            loop
               declare
                  Msg : constant String := Subscriber.Recv;
               begin
                  exit Recv_Loop when Msg = "END";
               end;
               Update_Nbr := Update_Nbr + 1;
            end loop Recv_Loop;
            Ada.Text_IO.Put_Line (-(+"Received %d updates"&Update_Nbr));
         end;

         Subscriber.Close;
         Sync_Client.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end SyncSub;
