with ZMQ;

package ZHelper is

   procedure Dump (S : ZMQ.Socket_Type'Class);
   --   Receives all message parts from socket, prints neatly

end ZHelper;
