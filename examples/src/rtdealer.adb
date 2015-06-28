--  ROUTER-to-DEALER example

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;
with ZHelper;

use type GNAT.Formatted_String.Formatted_String;
use type Ada.Real_Time.Time_Span;

procedure RTDealer is

   Nbr_Workers : constant := 10;
   Work_Time_Span : constant := 5;

   task type Worker_Task_Type is
      entry Start;
   end Worker_Task_Type;

   task body Worker_Task_Type is
   begin
      accept Start;

      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Worker : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_DEALER);
         Total : Natural := 0;
      begin
         ZHelper.Set_Id (Worker); --  Set a printable identity.
         Worker.Connect ("tcp://localhost:5671");

         Work_Loop :
         loop
            --  Tell the broker we're ready for work
            Worker.Send ("", Send_More => True);
            Worker.Send ("Hi Boss");

            --  Get workload from broker, until finished
            declare
               Dummy_Envelope : constant String := Worker.Recv; --  Envelope delimiter
               Workload : constant String := Worker.Recv;
            begin
               exit Work_Loop when Workload = "Fired!";
            end;

            --  Do some random work
            delay Duration (ZHelper.Rand_Of (0.001, 0.5));

            Total := Total + 1;
         end loop Work_Loop;
         Ada.Text_IO.Put_Line (-(+"Completed: %d tasks"&Total));

         Worker.Close;
         Context.Term;
      end;
   end Worker_Task_Type;


   --  While this example runs in a single process, that is only to make
   --  it easier to start and stop the example. Each thread has its own
   --  context and conceptually acts as a separate process.

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Broker : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
   begin
      Broker.Bind ("tcp://*:5671");

      declare
         Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         Tasks : array (1 .. Nbr_Workers) of Worker_Task_Type;
         Workers_Fired : Integer := 0;
      begin
         for T of Tasks loop
            T.Start;
         end loop;

         --  Run for five seconds and then tell workers to end
         Monitor_Loop :
         loop
            --  Next message gives us least recently used worker
            declare
               Identity : constant String := Broker.Recv;
               Unused_Delimiter : constant String := Broker.Recv;  --  Envelope delimiter
               Unused_Response : constant String := Broker.Recv;  --  Response from worker
            begin
               Broker.Send (Identity, Send_More => True);
               Broker.Send ("", Send_More => True);

               --  Encourage workers until it's time to fire them
               if (Ada.Real_Time.Clock - Start_Time) < Ada.Real_Time.Seconds (Work_Time_Span) then
                  Broker.Send ("Work harder");
               else
                  Broker.Send ("Fired!");
                  Workers_Fired := Workers_Fired + 1;
               end if;

               exit Monitor_Loop when Workers_Fired >= Nbr_Workers;
            end;
         end loop Monitor_Loop;
      end;

      Broker.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end RTDealer;
