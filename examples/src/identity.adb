--  Demonstrate request-reply identities

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;
with ZHelper;

use type GNAT.Formatted_String.Formatted_String;

procedure Identity is

   function Main return Ada.Command_Line.Exit_Status
   is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Sink : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
   begin
      Sink.Bind ("inproc://example");

      --  First allow 0MQ to set the identity
      declare
         Anonymous : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      begin
         Anonymous.Connect ("inproc://example");
         Anonymous.Send ("ROUTER uses a generated UUID");
         ZHelper.Dump (Sink);
         Anonymous.Close;
      end;

      --  Then set the identity ourselves
      declare
         Identified : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      begin
         Identified.Set_Sock_Opt (ZMQ.ZMQ_IDENTITY, "PEER2");
         Identified.Connect ("inproc://example");
         Identified.Send ("ROUTER socket uses REQ's socket identity");
         ZHelper.Dump (Sink);
         Identified.Close;
      end;

      Sink.Close;
      Context.Term;
      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end Identity;
