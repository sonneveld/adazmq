with System;
with Ada.Finalization;
with Interfaces.C;

private with ZMQ_Thin;

package ZMQ is

   ZMQ_Error : exception;

   type Context_Type is new Ada.Finalization.Limited_Controlled with private;
   type Message_Type is new Ada.Finalization.Limited_Controlled with private;
   type Socket_Type is new Ada.Finalization.Limited_Controlled with private;
   type Socket_Type_Access is access all Socket_Type'Class;

   type Poll_Item_Type is private;
   type Poll_Item_Array_Type is array (Natural range <>) of Poll_Item_Type;

   No_Socket : constant Socket_Type;


   type Socket_Types is
      (ZMQ_PAIR,
      ZMQ_PUB,
      ZMQ_SUB,
      ZMQ_REQ,
      ZMQ_REP,
      ZMQ_DEALER,
      ZMQ_ROUTER,
      ZMQ_PULL,
      ZMQ_PUSH,
      ZMQ_XPUB,
      ZMQ_XSUB,
      ZMQ_STREAM);

   type Socket_Option_Names is
      (ZMQ_AFFINITY,
      ZMQ_IDENTITY,
      ZMQ_SUBSCRIBE,
      ZMQ_UNSUBSCRIBE,
      ZMQ_RATE,
      ZMQ_RECOVERY_IVL,
      ZMQ_SNDBUF,
      ZMQ_RCVBUF,
      ZMQ_RCVMORE,
      ZMQ_FD,
      ZMQ_EVENTS,
      ZMQ_TYPE,
      ZMQ_LINGER,
      ZMQ_RECONNECT_IVL,
      ZMQ_BACKLOG,
      ZMQ_RECONNECT_IVL_MAX,
      ZMQ_MAXMSGSIZE,
      ZMQ_SNDHWM,
      ZMQ_RCVHWM,
      ZMQ_MULTICAST_HOPS,
      ZMQ_RCVTIMEO,
      ZMQ_SNDTIMEO,
      ZMQ_LAST_ENDPOINT,
      ZMQ_ROUTER_MANDATORY,
      ZMQ_TCP_KEEPALIVE,
      ZMQ_TCP_KEEPALIVE_CNT,
      ZMQ_TCP_KEEPALIVE_IDLE,
      ZMQ_TCP_KEEPALIVE_INTVL,
      ZMQ_TCP_ACCEPT_FILTER,
      ZMQ_IMMEDIATE,
      ZMQ_XPUB_VERBOSE,
      ZMQ_ROUTER_RAW,
      ZMQ_IPV6,
      ZMQ_MECHANISM,
      ZMQ_PLAIN_SERVER,
      ZMQ_PLAIN_USERNAME,
      ZMQ_PLAIN_PASSWORD,
      ZMQ_CURVE_SERVER,
      ZMQ_CURVE_PUBLICKEY,
      ZMQ_CURVE_SECRETKEY,
      ZMQ_CURVE_SERVERKEY,
      ZMQ_PROBE_ROUTER,
      ZMQ_REQ_CORRELATE,
      ZMQ_REQ_RELAXED,
      ZMQ_CONFLATE,
      ZMQ_ZAP_DOMAIN);


   --  Version
   --  ------------------------------------------------------------------------

   procedure Version (Major : out Natural; Minor : out Natural; Patch : out Natural);


   --  Error Handling
   --  ------------------------------------------------------------------------

   function Errno return Integer;
   function Strerror (Err_Num : Integer) return String;
   procedure Raise_Errno;
   pragma No_Return (Raise_Errno);


   --  Context
   --  ------------------------------------------------------------------------

   type Context_Option_Names is (ZMQ_IO_THREADS, ZMQ_MAX_SOCKETS, ZMQ_IPV6);

   overriding procedure Initialize (Cxt : in out Context_Type);
   overriding procedure Finalize (Cxt : in out Context_Type);

   procedure Setup (Cxt : in out Context_Type);
   function New_Context return Context_Type;

   procedure Term (Context : in out Context_Type);
   procedure Shutdown (Context : Context_Type);
   procedure Set (Context : Context_Type; Name : Context_Option_Names; Value : Natural);
   function Get (Context : Context_Type; Name : Context_Option_Names) return Natural;
   function New_Socket (Context : Context_Type; Instance_Type : Socket_Types) return Socket_Type'Class;


   --  Message_Type
   --  ------------------------------------------------------------------------

   overriding procedure Initialize (M : in out Message_Type);
   overriding procedure Finalize (M : in out Message_Type);

   procedure Setup (M : in out Message_Type; Size : Integer := 0);
   function Create_Message (Size : Integer := 0) return Message_Type;
   procedure Setup (M : in out Message_Type; Value : String);
   function Create_Message (Value : String) return Message_Type;

   procedure Send (M : in out Message_Type; S : Socket_Type'Class; Do_Not_Wait : Boolean := False; Send_More : Boolean := False);
   procedure Recv (M : in out Message_Type; S : Socket_Type'Class; Do_Not_Wait : Boolean := False);
   procedure Close (M : in out Message_Type);
   procedure Move (Dest : in out Message_Type; Src : in out Message_Type);
   procedure Copy (Dest : in out Message_Type; Src : in out Message_Type);
   function Data (M : in out Message_Type) return String;
   --  TODO data returning other types..
   function Size (M : in out Message_Type) return Natural;
   function More (M : in out Message_Type) return Boolean;
   function Src_FD (M : in out Message_Type) return Integer;
   function Shared (M : in out Message_Type) return Boolean;


   --  Socket
   --  ------------------------------------------------------------------------

   overriding procedure Initialize (S : in out Socket_Type);
   overriding procedure Finalize (S : in out Socket_Type);

   procedure Setup (S : in out Socket_Type; Context : Context_Type'Class; Instance_Type : Socket_Types);

   procedure Close (S : in out Socket_Type);
   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : Integer);
   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : Long_Long_Integer);
   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : String);
   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return Integer;
   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return Long_Long_Integer;
   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return String;
   procedure Bind (S : Socket_Type; Address : String);
   procedure Connect (S : Socket_Type; Address : String);
   procedure Unbind (S : Socket_Type; Address : String);
   procedure Disconnect (S : Socket_Type; Address : String);

   --  TODO: What about an array of flags?
   procedure Send (S : Socket_Type; Buf : String; Do_Not_Wait : Boolean := False; Send_More : Boolean := False);
   --  TODO: Recv could have a version that out's an error.. and another that out's MORE
   function Recv (S : Socket_Type; Do_Not_Wait : Boolean := False) return String;



   --  Polling
   --  ------------------------------------------------------------------------

   --  Create an array of Poll Items and call Poll on it.  The array will be modified.
   --  No good way to get sockets or FDs back from poll item yet.  Right now, remember the index in the array.
   procedure Setup (PE : in out Poll_Item_Type; S : Socket_Type'Class; Poll_In : Boolean := False; Poll_Out : Boolean := False);
   function New_Poll_Item (S : Socket_Type'Class; Poll_In : Boolean := False; Poll_Out : Boolean := False) return Poll_Item_Type;
   procedure Setup (PE : in out Poll_Item_Type; FD : Integer; Poll_In : Boolean := False; Poll_Out : Boolean := False; Poll_Error : Boolean := False);
   function New_Poll_Item (FD : Integer; Poll_In : Boolean := False; Poll_Out : Boolean := False; Poll_Error : Boolean := False) return Poll_Item_Type;
   function Is_Readable (PE : Poll_Item_Type) return Boolean;
   function Is_Writable (PE : Poll_Item_Type) return Boolean;
   function Has_Error (PE : Poll_Item_Type) return Boolean;

   function Poll (Entries : in out Poll_Item_Array_Type; Timeout : Long_Long_Integer := -1) return Natural;
   procedure Poll (Entries : in out Poll_Item_Array_Type; Timeout : Long_Long_Integer := -1);


   --  Proxy
   --  ------------------------------------------------------------------------

   procedure Proxy (Frontend : Socket_Type'Class; Backend : Socket_Type'Class; Capture : Socket_Type'Class := ZMQ.No_Socket);
   procedure Proxy_Steerable (Frontend : Socket_Type'Class; Backend : Socket_Type'Class; Capture : Socket_Type'Class := ZMQ.No_Socket; Control : Socket_Type'Class := ZMQ.No_Socket);


private

   type Context_Type is new Ada.Finalization.Limited_Controlled with record
      Impl_Ptr : System.Address := System.Null_Address;
   end record;

   type Message_Type is new Ada.Finalization.Limited_Controlled with record
      Msg_Internals : aliased ZMQ_Thin.zmq_msg_t;
   end record;

   type Socket_Type is new Ada.Finalization.Limited_Controlled with record
      Impl_Ptr : System.Address := System.Null_Address;
   end record;

   No_Socket : constant Socket_Type := Socket_Type'(Ada.Finalization.Limited_Controlled with Impl_Ptr => System.Null_Address);


   type Poll_Item_Type is record
      S_Impl_Ptr : System.Address := System.Null_Address;
      FD : Interfaces.C.int := 0;

      Poll_In : Boolean := False;
      Poll_Out : Boolean := False;
      Poll_Error : Boolean := False;

      Result_In : Boolean := False;
      Result_Out : Boolean := False;
      Result_Error : Boolean := False;
   end record;

end ZMQ;
