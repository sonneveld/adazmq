--  Task sink
--  Binds PULL socket to tcp://localhost:5558
--  Collects results from workers via that socket

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;
with Ada.Calendar;

with ZMQ;

procedure TaskSink2 is

   use type Ada.Calendar.Time;
   use type GNAT.Formatted_String.Formatted_String;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin

      declare
      --  Prepare our context and socket
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Receiver : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PULL);
         Controller : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUB);
      begin
         Receiver.Bind ("tcp://*:5558");
         Controller.Bind ("tcp://*:5559");

         --  Wait for start of batch
         declare
            Dummy : String := Receiver.Recv;
         begin
            null;
         end;

         declare
            --  Start our clock now
            Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            --  Process 100 confirmations
            for Task_Nbr in 1 .. 100 loop
               declare
                  Dummy : String := Receiver.Recv;
               begin
                  null;
                  if Task_Nbr mod 10 = 0 then
                     Ada.Text_IO.Put (":");
                  else
                     Ada.Text_IO.Put (".");
                  end if;
                  Ada.Text_IO.Flush;
               end;
            end loop;

            --  Calculate and report duration of batch
            Ada.Text_IO.Put_Line ("");
            Ada.Text_IO.Put_Line (-(+"Total elapsed time: %d msec" & Integer ((Ada.Calendar.Clock - Start_Time) * 1000.0)));
         end;

         --  Send kill signal to workers
         Controller.Send ("KILL");

         Receiver.Close;
         Controller.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end TaskSink2;
