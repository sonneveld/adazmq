with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.Formatted_String;

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

   procedure Set_Id (S : ZMQ.Socket_Type'Class)
   is
      Identity : constant String := -(+"%04X-%04X"&Rand_Of (1, 16#10000#)&Rand_Of (1, 16#10000#));
   begin
      S.Set_Sock_Opt (ZMQ.ZMQ_IDENTITY, Identity);
   end Set_Id;

end ZHelper;
