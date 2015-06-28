with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.Formatted_String;
with GNAT.Task_Lock;

use type GNAT.Formatted_String.Formatted_String;

package body ZHelper is

   function Rand_Of (First : Integer; Last : Integer) return Integer is
      subtype Range_T is Integer range First .. Last;
      package Random is new Ada.Numerics.Discrete_Random (Range_T);
      Seed : Random.Generator;
   begin
      Random.Reset (Seed);
      return Integer (Random.Random (Seed));
   end Rand_Of;

   function Rand_Of (First : Float; Last : Float) return Float is
      Seed : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset (Seed);
      return Ada.Numerics.Float_Random.Random (Seed) * (Last - First) + First;
   end Rand_Of;

   function Is_Text (Data : String) return Boolean
   is
   begin
      for X of Data loop
         if not (X in Character'Val (32) .. Character'Val (126)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Text;

   procedure Dump (S : ZMQ.Socket_Type'Class)
   is
      M : ZMQ.Message_Type := ZMQ.Create_Message;
   begin
      Ada.Text_IO.Put_Line ("----------------------------------------");
      --  Process all parts of the message
      Read_Loop :
      loop
         M.Recv (S);

         --  Dump the message as text or binary
         declare
            Data : constant String := M.Data;
            Data_Is_Text : constant Boolean := Is_Text (Data);
         begin
            Ada.Text_IO.Put (-(+"[%03d] " & Integer (Data'Length)));
            for X of Data loop
               if Data_Is_Text then
                  Ada.Text_IO.Put (-(+"%c" & X));
               else
                  Ada.Text_IO.Put (-(+"%02X " & Integer (Character'Pos (X))));
               end if;
            end loop;
            Ada.Text_IO.Put_Line ("");
         end;

         exit Read_Loop when not M.More;  --  Multipart detection
      end loop Read_Loop;
   end Dump;

   --  Id_Count and Task_Lock malarkey ensure that ids are unique.

   Next_Id_Value : Natural := 1;

   function Set_Id (S : ZMQ.Socket_Type'Class) return String
   is
      Id_Base : Natural;
   begin
      begin
         GNAT.Task_Lock.Lock;
         Id_Base := Next_Id_Value;
         Next_Id_Value := Next_Id_Value + 1;
         GNAT.Task_Lock.Unlock;
      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end;

      declare
         Identity : constant String := -(+"%04X-%04X"&Rand_Of (1, 16#10000#)&Id_Base);
      begin
         S.Set_Sock_Opt (ZMQ.ZMQ_IDENTITY, Identity);
         return Identity;
      end;
   end Set_Id;

   procedure Set_Id (S : ZMQ.Socket_Type'Class)
   is
      Dummy : String := Set_Id (S);
   begin
      null;
   end Set_Id;

end ZHelper;
