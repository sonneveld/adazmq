--  Synchronized publisher

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure SyncPub is

   use type GNAT.Formatted_String.Formatted_String;

   Subscribers_Expected : constant := 10; --  We wait for 10 subscribers

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Socket to talk to clients
         Publisher : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUB);
         --  Socket to receive signals
         Sync_Service : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REP);
      begin
         Publisher.Set_Sock_Opt (ZMQ.ZMQ_SNDHWM, Integer (1100000));
         Publisher.Bind ("tcp://*:5561");

         Sync_Service.Bind ("tcp://*:5562");

         --  Get synchronization from subscribers
         Ada.Text_IO.Put_Line ("Waiting for subscribers");
         for Subscribers in 1 .. Subscribers_Expected loop
            --  - wait for synchronization request
            declare
               Unused_Msg : String := Sync_Service.Recv;
            begin
               null;
            end;
            --  - send synchronization reply
            Sync_Service.Send ("");
         end loop;

         --  Now broadcast exactly 1M updates followed by END
         Ada.Text_IO.Put_Line ("Broadcasting messages");
         for Update_Nbr in 1 .. 1000000 loop
            Publisher.Send ("Rhubarb");
         end loop;

         Publisher.Send ("END");

         Publisher.Close;
         Sync_Service.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end SyncPub;
