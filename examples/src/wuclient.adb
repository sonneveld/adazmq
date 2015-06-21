--  Weather update client
--  Connects SUB socket to tcp://localhost:5556
--  Collects weather updates and finds avg temp in zipcode

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;

procedure WUClient is

   use type GNAT.Formatted_String.Formatted_String;

   Number_Iterations : constant := 100;

   type Zip_Code_T is range 0 .. 100000;
   type Temperature_T is range -80 .. 135;
   type Rel_Humidity_T is range 10 .. 60;

   package Zip_Code_Text_IO is new Ada.Text_IO.Integer_IO (Zip_Code_T);
   package Temperature_Text_IO is new Ada.Text_IO.Integer_IO (Temperature_T);
   package Rel_Humidity_Text_IO is new Ada.Text_IO.Integer_IO (Rel_Humidity_T);

   function Client (Zip_Code : Zip_Code_T := 10001) return Ada.Command_Line.Exit_Status
   is
   begin
      --  Socket to talk to server
      Ada.Text_IO.Put_Line ("Collecting updates from weather server...");

      declare
         --  Prepare our context and publisher
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Subscriber : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_SUB);
      begin
         Subscriber.Connect ("tcp://localhost:5556");

         --  Subscribe to zipcode, default is NYC, 10001
         declare
            Filter : constant String := -(+"%05d "&Integer (Zip_Code));
         begin
            Subscriber.Set_Sock_Opt (ZMQ.ZMQ_SUBSCRIBE, Filter);
         end;

         --  Process 100 updates
         declare
            Total_Temp : Long_Integer := 0;
            Average : Long_Integer;
         begin
            for Update_Nbr in 1 .. Number_Iterations loop
               declare
                  Buf : constant String := Subscriber.Recv;
                  Zip_Code :   Zip_Code_T;
                  Temperature :   Temperature_T;
                  Rel_Humidity :   Rel_Humidity_T;
                  Last : Positive;
               begin
                  Zip_Code_Text_IO.Get (From => Buf, Item => Zip_Code, Last => Last);
                  Temperature_Text_IO.Get (From => Buf (Last+1 .. Buf'Last), Item => Temperature, Last => Last);
                  Rel_Humidity_Text_IO.Get (From => Buf (Last+1 .. Buf'Last), Item => Rel_Humidity, Last => Last);
                  Total_Temp := Total_Temp + Long_Integer (Temperature);
               end;
            end loop;
            Average := Total_Temp / Long_Integer (Number_Iterations);
            Ada.Text_IO.Put_Line (-(+"Average temperature for zipcode '%05d' was %dF" & Integer (Zip_Code) & Average));
         end;

         Subscriber.Close;
         Context.Term;
      end;
      return 0;
   end Client;

   function Main return Ada.Command_Line.Exit_Status
   is
   begin
      if Ada.Command_Line.Argument_Count = 1 then
         return Client (Zip_Code_T'Value (Ada.Command_Line.Argument(1)));
      elsif Ada.Command_Line.Argument_Count = 0 then
         return Client;
      else
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Usage: wuclient [zipcode]");
         return 1;
      end if;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end WUClient;
