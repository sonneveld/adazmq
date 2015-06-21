--  Task ventilator
--  Binds PUSH socket to tcp://localhost:5557
--  Sends batch of tasks to workers via that socket

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Formatted_String;

with ZMQ;

procedure TaskVent is

   use type GNAT.Formatted_String.Formatted_String;

   type Workload_T is range 1 .. 100;
   package Random_Workload is new Ada.Numerics.Discrete_Random (Workload_T);

   function Main return Ada.Command_Line.Exit_Status
   is
      Random_Workload_Seed : Random_Workload.Generator;
   begin



      declare
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         --  Socket to send messages on
         Sender : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUSH);
         --  Socket to send start of batch message on
         Sink : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUSH);
      begin
         Sender.Bind ("tcp://*:5557");
         Sink.Connect ("tcp://localhost:5558");

         Ada.Text_IO.Put ("Press Enter when the workers are ready: ");
         declare
            Dummy : String := Ada.Text_IO.Get_Line;
         begin
            null;
         end;
         Ada.Text_IO.Put_Line ("Sending tasks to workers...");

         --  The first message is "0" and signals start of batch
         Sink.Send ("0");

         --  --  Initialize random number generator
         Random_Workload.Reset (Random_Workload_Seed);

         --  Send 100 tasks
         declare
            total_msec : Integer := 0;  --  Total expected cost in msecs
         begin
            for Task_Nbr in 1 .. 100 loop
               declare
                  --  Random workload from 1 to 100msecs
                  Workload : constant Workload_T := Random_Workload.Random (Random_Workload_Seed);
               begin
                  total_msec := total_msec + Integer (Workload);
                  Sender.Send (-(+"%d"&Integer (Workload)));
               end;
            end loop;
            Ada.Text_IO.Put_Line (-(+"Total expected cost: %d msec"&Integer (total_msec)));

         end;


         Sink.Close;
         Sender.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end TaskVent;
