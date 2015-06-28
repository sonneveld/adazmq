with ZMQ;

package ZHelper is

   function Rand_Of (First : Integer; Last : Integer) return Integer;
   function Rand_Of (First : Float; Last : Float) return Float;
   --  Provide random number from First .. Last

   procedure Dump (S : ZMQ.Socket_Type'Class);
   --  Receives all message parts from socket, prints neatly

   function Set_Id (S : ZMQ.Socket_Type'Class) return String;
   procedure Set_Id (S : ZMQ.Socket_Type'Class);
   --  Set simple random printable identity on socket

end ZHelper;
