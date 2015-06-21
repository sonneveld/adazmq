--  Weather update server
--  Binds PUB socket to tcp://*:5556
--  Publishes random weather updates

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with GNAT.Formatted_String;

with ZMQ;

procedure WUServer is

   use type GNAT.Formatted_String.Formatted_String;

   type Zip_Code_T is range 0 .. 100000;
   type Temperature_T is range -80 .. 135;
   type Rel_Humidity_T is range 10 .. 60;
   package Random_Zip_Code is new Ada.Numerics.Discrete_Random (Zip_Code_T);
   package Random_Temperature is new Ada.Numerics.Discrete_Random (Temperature_T);
   package Random_Rel_Humidity is new Ada.Numerics.Discrete_Random (Rel_Humidity_T);

   function Main return Ada.Command_Line.Exit_Status
   is
      Random_Zip_Code_Seed : Random_Zip_Code.Generator;
      Random_Temperature_Seed : Random_Temperature.Generator;
      Random_Rel_Humidity_Seed : Random_Rel_Humidity.Generator;
   begin

      --  Initialize random number generator
      Random_Zip_Code.Reset (Random_Zip_Code_Seed);
      Random_Temperature.Reset (Random_Temperature_Seed);
      Random_Rel_Humidity.Reset (Random_Rel_Humidity_Seed);

      declare
         --  Prepare our context and publisher
         Context : ZMQ.Context_Type := ZMQ.New_Context;
         Publisher : constant ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_PUB);
      begin
         Publisher.Bind ("tcp://*:5556");

         loop
            --  Get values that will fool the boss
            declare
               Zip_Code : constant Zip_Code_T := Random_Zip_Code.Random (Random_Zip_Code_Seed);
               Temperature : constant Temperature_T := Random_Temperature.Random (Random_Temperature_Seed);
               Rel_Humidity : constant Rel_Humidity_T := Random_Rel_Humidity.Random (Random_Rel_Humidity_Seed);
            begin
               --  Send message to all subscribers
               Publisher.Send (-(+"%05d %d %d"&Integer (Zip_Code) & Integer (Temperature) & Integer (Rel_Humidity)));
            end;
         end loop;

         Publisher.Close;
         Context.Term;
      end;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end WUServer;
