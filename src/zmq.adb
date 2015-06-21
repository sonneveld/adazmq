with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

with ZMQ_Constants;

package body ZMQ is

   use type System.Address;
   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   use type Interfaces.C.unsigned;


   -- Conversion

   function Convert is new Ada.Unchecked_Conversion
      (Source => Interfaces.C.size_t,
      Target => Interfaces.C.unsigned_long);

   function Convert is new Ada.Unchecked_Conversion
      (Source => System.Address,
      Target => Interfaces.C.Strings.chars_ptr);

   function Convert is new Ada.Unchecked_Conversion
      (Source => Interfaces.C.Strings.chars_ptr,
      Target => System.Address);

   function Convert (Value : Context_Type'Class) return System.Address is (Value.Impl_Ptr);

   function Convert (Value : Context_Option_Names) return Interfaces.C.int
   is
   begin
      case Value is
         when ZMQ_IO_THREADS => return ZMQ_Constants.ZMQ_IO_THREADS;
         when ZMQ_MAX_SOCKETS => return ZMQ_Constants.ZMQ_MAX_SOCKETS;
         when ZMQ_IPV6 => return ZMQ_Constants.ZMQ_IPV6;
      end case;
   end Convert;

   function Convert (Value : Socket_Type'Class) return System.Address is (Value.Impl_Ptr);

   function Convert (Value : Socket_Types) return Interfaces.C.int
   is
   begin
      case Value is
         when ZMQ_PAIR => return ZMQ_Constants.ZMQ_PAIR;
         when ZMQ_PUB => return ZMQ_Constants.ZMQ_PUB;
         when ZMQ_SUB => return ZMQ_Constants.ZMQ_SUB;
         when ZMQ_REQ => return ZMQ_Constants.ZMQ_REQ;
         when ZMQ_REP => return ZMQ_Constants.ZMQ_REP;
         when ZMQ_DEALER => return ZMQ_Constants.ZMQ_DEALER;
         when ZMQ_ROUTER => return ZMQ_Constants.ZMQ_ROUTER;
         when ZMQ_PULL => return ZMQ_Constants.ZMQ_PULL;
         when ZMQ_PUSH => return ZMQ_Constants.ZMQ_PUSH;
         when ZMQ_XPUB => return ZMQ_Constants.ZMQ_XPUB;
         when ZMQ_XSUB => return ZMQ_Constants.ZMQ_XSUB;
         when ZMQ_STREAM => return ZMQ_Constants.ZMQ_STREAM;
      end case;
   end Convert;

   function Convert (Value : Socket_Option_Names) return Interfaces.C.int
   is
   begin
      case Value is
         when ZMQ_AFFINITY => return ZMQ_Constants.ZMQ_AFFINITY;
         when ZMQ_IDENTITY => return ZMQ_Constants.ZMQ_IDENTITY;
         when ZMQ_SUBSCRIBE => return ZMQ_Constants.ZMQ_SUBSCRIBE;
         when ZMQ_UNSUBSCRIBE => return ZMQ_Constants.ZMQ_UNSUBSCRIBE;
         when ZMQ_RATE => return ZMQ_Constants.ZMQ_RATE;
         when ZMQ_RECOVERY_IVL => return ZMQ_Constants.ZMQ_RECOVERY_IVL;
         when ZMQ_SNDBUF => return ZMQ_Constants.ZMQ_SNDBUF;
         when ZMQ_RCVBUF => return ZMQ_Constants.ZMQ_RCVBUF;
         when ZMQ_RCVMORE => return ZMQ_Constants.ZMQ_RCVMORE;
         when ZMQ_FD => return ZMQ_Constants.ZMQ_FD;
         when ZMQ_EVENTS => return ZMQ_Constants.ZMQ_EVENTS;
         when ZMQ_TYPE => return ZMQ_Constants.ZMQ_TYPE;
         when ZMQ_LINGER => return ZMQ_Constants.ZMQ_LINGER;
         when ZMQ_RECONNECT_IVL => return ZMQ_Constants.ZMQ_RECONNECT_IVL;
         when ZMQ_BACKLOG => return ZMQ_Constants.ZMQ_BACKLOG;
         when ZMQ_RECONNECT_IVL_MAX => return ZMQ_Constants.ZMQ_RECONNECT_IVL_MAX;
         when ZMQ_MAXMSGSIZE => return ZMQ_Constants.ZMQ_MAXMSGSIZE;
         when ZMQ_SNDHWM => return ZMQ_Constants.ZMQ_SNDHWM;
         when ZMQ_RCVHWM => return ZMQ_Constants.ZMQ_RCVHWM;
         when ZMQ_MULTICAST_HOPS => return ZMQ_Constants.ZMQ_MULTICAST_HOPS;
         when ZMQ_RCVTIMEO => return ZMQ_Constants.ZMQ_RCVTIMEO;
         when ZMQ_SNDTIMEO => return ZMQ_Constants.ZMQ_SNDTIMEO;
         when ZMQ_LAST_ENDPOINT => return ZMQ_Constants.ZMQ_LAST_ENDPOINT;
         when ZMQ_ROUTER_MANDATORY => return ZMQ_Constants.ZMQ_ROUTER_MANDATORY;
         when ZMQ_TCP_KEEPALIVE => return ZMQ_Constants.ZMQ_TCP_KEEPALIVE;
         when ZMQ_TCP_KEEPALIVE_CNT => return ZMQ_Constants.ZMQ_TCP_KEEPALIVE_CNT;
         when ZMQ_TCP_KEEPALIVE_IDLE => return ZMQ_Constants.ZMQ_TCP_KEEPALIVE_IDLE;
         when ZMQ_TCP_KEEPALIVE_INTVL => return ZMQ_Constants.ZMQ_TCP_KEEPALIVE_INTVL;
         when ZMQ_TCP_ACCEPT_FILTER => return ZMQ_Constants.ZMQ_TCP_ACCEPT_FILTER;
         when ZMQ_IMMEDIATE => return ZMQ_Constants.ZMQ_IMMEDIATE;
         when ZMQ_XPUB_VERBOSE => return ZMQ_Constants.ZMQ_XPUB_VERBOSE;
         when ZMQ_ROUTER_RAW => return ZMQ_Constants.ZMQ_ROUTER_RAW;
         when ZMQ_IPV6 => return ZMQ_Constants.ZMQ_IPV6;
         when ZMQ_MECHANISM => return ZMQ_Constants.ZMQ_MECHANISM;
         when ZMQ_PLAIN_SERVER => return ZMQ_Constants.ZMQ_PLAIN_SERVER;
         when ZMQ_PLAIN_USERNAME => return ZMQ_Constants.ZMQ_PLAIN_USERNAME;
         when ZMQ_PLAIN_PASSWORD => return ZMQ_Constants.ZMQ_PLAIN_PASSWORD;
         when ZMQ_CURVE_SERVER => return ZMQ_Constants.ZMQ_CURVE_SERVER;
         when ZMQ_CURVE_PUBLICKEY => return ZMQ_Constants.ZMQ_CURVE_PUBLICKEY;
         when ZMQ_CURVE_SECRETKEY => return ZMQ_Constants.ZMQ_CURVE_SECRETKEY;
         when ZMQ_CURVE_SERVERKEY => return ZMQ_Constants.ZMQ_CURVE_SERVERKEY;
         when ZMQ_PROBE_ROUTER => return ZMQ_Constants.ZMQ_PROBE_ROUTER;
         when ZMQ_REQ_CORRELATE => return ZMQ_Constants.ZMQ_REQ_CORRELATE;
         when ZMQ_REQ_RELAXED => return ZMQ_Constants.ZMQ_REQ_RELAXED;
         when ZMQ_CONFLATE => return ZMQ_Constants.ZMQ_CONFLATE;
         when ZMQ_ZAP_DOMAIN => return ZMQ_Constants.ZMQ_ZAP_DOMAIN;
      end case;
   end Convert;


   procedure Version (Major : out Natural; Minor : out Natural; Patch : out Natural)
   is
      c_major : aliased Interfaces.C.int;
      c_minor : aliased Interfaces.C.int;
      c_patch : aliased Interfaces.C.int;
   begin
      ZMQ_Thin.zmq_version (c_major'Access, c_minor'Access, c_patch'Access);
      Major := Natural (c_major);
      Minor := Natural (c_minor);
      Patch := Natural (c_patch);
   end Version;


   function Errno return Integer
   is
   begin
      return Integer (ZMQ_Thin.zmq_errno);
   end Errno;

   function Strerror (Err_Num : Integer) return String
   is
      c_errnum : constant Interfaces.C.int := Interfaces.C.int (Err_Num);
      c_strerror : constant Interfaces.C.Strings.chars_ptr := ZMQ_Thin.zmq_strerror (c_errnum);
   begin
      return Interfaces.C.Strings.Value (c_strerror);
   end Strerror;

   procedure Raise_Errno is
      Error_Number : constant Integer := Errno;
   begin
      raise ZMQ_Error with "[" & Error_Number'Img & "] " & Strerror (Error_Number);
   end Raise_Errno;
   pragma No_Return (Raise_Errno);



   --  Context
   --  ------------------------------------------------------------------------

   overriding procedure Initialize (Cxt : in out Context_Type) is
   begin
      Setup (Cxt);
   end Initialize;

   overriding procedure Finalize (Cxt : in out Context_Type) is
   begin
      if Cxt.Impl_Ptr /= System.Null_Address then
         Term (Cxt);
      end if;
   end Finalize;

   procedure Setup (Cxt : in out Context_Type) is
      c_cxt : constant System.Address := ZMQ_Thin.zmq_ctx_new;
   begin
      if c_cxt /= System.Null_Address then
         Cxt.Impl_Ptr := c_cxt;
      else
         Raise_Errno;
      end if;
   end Setup;

   function New_Context return Context_Type
   is
   begin
      return Cxt : Context_Type do
         Setup (Cxt);
      end return;
   end New_Context;

   procedure Term (Context : in out Context_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_ctx_term (Convert (Context));
      if c_res = 0 then
         Context.Impl_Ptr := System.Null_Address;
         return;
      else
         Raise_Errno;
      end if;
   end Term;

   procedure Shutdown (Context : Context_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_ctx_shutdown (Convert (Context));
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Shutdown;

   procedure Set (Context : Context_Type; Name : Context_Option_Names; Value : Natural)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_ctx_set (Convert (Context), Convert (Name), Interfaces.C.int (Value));
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Set;

   function Get (Context : Context_Type; Name : Context_Option_Names) return Natural
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_ctx_get (Convert (Context), Convert (Name));
      if c_res >= 0 then
         return Natural (c_res);
      else
         Raise_Errno;
      end if;
   end Get;

   function New_Socket (Context : Context_Type; Instance_Type : Socket_Types) return Socket_Type'Class
   is
   begin
      return S : Socket_Type do
         Setup (S, Context, Instance_Type);
      end return;
   end New_Socket;



   --  Message_Type
   --  ------------------------------------------------------------------------

   overriding procedure Initialize (M : in out Message_Type) is
   begin
      Setup (M);
   end Initialize;

   overriding procedure Finalize (M : in out Message_Type) is
   begin
      Close (M);
   end Finalize;


   procedure Setup (M : in out Message_Type; Size : Integer := 0)
   is
      c_size : constant Interfaces.C.size_t := Interfaces.C.size_t (Size);
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_init_size (M.Msg_Internals'Access, c_size);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Setup;

   function Create_Message (Size : Integer := 0) return Message_Type
   is
   begin
      return M : Message_Type do
         Setup (M, Size);
      end return;
   end Create_Message;

   procedure Setup (M : in out Message_Type; Value : String)
   is
      c_size : constant Interfaces.C.size_t := Interfaces.C.size_t (Value'Length);
      c_res : Interfaces.C.int;

      c_data : System.Address;

   begin
      c_res := ZMQ_Thin.zmq_msg_init_size (M.Msg_Internals'Access, c_size);
      if c_res /= 0 then
         Raise_Errno;
      end if;

      c_data := ZMQ_Thin.zmq_msg_data (M.Msg_Internals'Access);
      if c_data = System.Null_Address then
         Raise_Errno;
      end if;

      declare
         Data_String : String (1 .. Value'Length);
         for Data_String'Address use c_data;
      begin
         Data_String (1 .. Value'Length) := Value (Value'First .. Value'Last);
      end;
   end Setup;

   function Create_Message (Value : String) return Message_Type
   is
   begin
      return M : Message_Type do
         Setup (M, Value);
      end return;
   end Create_Message;

   procedure Send (M : in out Message_Type; S : Socket_Type'Class; Do_Not_Wait : Boolean := False; Send_More : Boolean := False)
   is
      Expected_Length : constant Integer := Size (M);

      c_flags : Interfaces.C.unsigned := 0;

      c_res : Interfaces.C.int;
   begin
      c_flags := 0;
      if Do_Not_Wait then
         c_flags := c_flags or ZMQ_Constants.ZMQ_DONTWAIT;
      end if;
      if Send_More then
         c_flags := c_flags or ZMQ_Constants.ZMQ_SNDMORE;
      end if;

      c_res := ZMQ_Thin.zmq_msg_send (M.Msg_Internals'Access, Convert (S), c_flags);
      if c_res >= 0 and then Interfaces.C.int (Expected_Length) = c_res then
         return;
      elsif c_res >= 0 then
         raise ZMQ_Error with "Entire Message_Type wasn't sent.";
      else
         Raise_Errno;
      end if;
   end Send;

   procedure Recv (M : in out Message_Type; S : Socket_Type'Class; Do_Not_Wait : Boolean := False)
   is
      c_flags : Interfaces.C.unsigned := 0;

      c_res : Interfaces.C.int;
   begin
      c_flags := 0;
      if Do_Not_Wait then
         c_flags := c_flags or ZMQ_Constants.ZMQ_DONTWAIT;
      end if;

      c_res := ZMQ_Thin.zmq_msg_recv (M.Msg_Internals'Access, Convert (S), c_flags);
      if c_res >= 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Recv;

   procedure Close (M : in out Message_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_close (M.Msg_Internals'Access);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Close;

   procedure Move (Dest : in out Message_Type; Src : in out Message_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_move (Dest.Msg_Internals'Access, Src.Msg_Internals'Access);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Move;

   procedure Copy (Dest : in out Message_Type; Src : in out Message_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_copy (Dest.Msg_Internals'Access, Src.Msg_Internals'Access);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Copy;

   --  TODO date returning other types..

   function Data (M : in out Message_Type) return String
   is
      c_data : System.Address;
      c_size : Interfaces.C.size_t;
   begin
      c_data := ZMQ_Thin.zmq_msg_data (M.Msg_Internals'Access);
      if c_data = System.Null_Address then
         Raise_Errno;
      end if;

      c_size := ZMQ_Thin.zmq_msg_size (M.Msg_Internals'Access);

      return Interfaces.C.Strings.Value (Convert (c_data), c_size);
   end Data;


   function Size (M : in out Message_Type) return Natural
   is
      c_res : Interfaces.C.size_t;
   begin
      c_res := ZMQ_Thin.zmq_msg_size (M.Msg_Internals'Access);
      return Natural (c_res);
   end Size;

   function More (M : in out Message_Type) return Boolean
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_more (M.Msg_Internals'Access);
      return c_res /= 0;
   end More;

   function Src_FD (M : in out Message_Type) return Integer
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_get (M.Msg_Internals'Access, ZMQ_Constants.ZMQ_SRCFD);
      if c_res >= 0 then
         return Integer (c_res);
      else
         Raise_Errno;
      end if;
   end Src_FD;

   function Shared (M : in out Message_Type) return Boolean
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_msg_get (M.Msg_Internals'Access, ZMQ_Constants.ZMQ_SHARED);
      if c_res >= 0 then
         return c_res /= 0;
      else
         Raise_Errno;
      end if;
   end Shared;



   --  Socket
   --  ------------------------------------------------------------------------

   overriding procedure Initialize (S : in out Socket_Type)
   is
   begin
      S.Impl_Ptr := System.Null_Address;
   end Initialize;

   overriding procedure Finalize (S : in out Socket_Type)
   is
   begin
      if S.Impl_Ptr /= System.Null_Address then
         Close (S);
      end if;
   end Finalize;

   procedure Setup (S : in out Socket_Type; Context : Context_Type'Class; Instance_Type : Socket_Types)
   is
      c_socket : System.Address;
   begin
      c_socket := ZMQ_Thin.zmq_socket (Convert (Context), Convert (Instance_Type));
      if c_socket /= System.Null_Address then
         S.Impl_Ptr := c_socket;
      else
         Raise_Errno;
      end if;
   end Setup;

   procedure Close (S : in out Socket_Type)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_close (Convert (S));
      if c_res = 0 then
         S.Impl_Ptr := System.Null_Address;
         return;
      else
         Raise_Errno;
      end if;
   end Close;

   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : Integer)
   is
      c_optval : constant Interfaces.C.int := Interfaces.C.int (Option_Value);
      c_optval_ptr : constant System.Address := c_optval'Address;
      c_optvallen : constant Interfaces.C.size_t := c_optval'Size / 8;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_setsockopt (Convert (S), Convert (Option), c_optval_ptr, c_optvallen);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Set_Sock_Opt;

   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : Long_Long_Integer)
   is
      c_optval : constant Interfaces.C.long := Interfaces.C.long (Option_Value);
      c_optval_ptr : constant System.Address := c_optval'Address;
      c_optvallen : constant Interfaces.C.size_t := c_optval'Size / 8;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_setsockopt (Convert (S), Convert (Option), c_optval_ptr, c_optvallen);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Set_Sock_Opt;

   procedure Set_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names; Option_Value : String)
   is
      c_optval : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Option_Value);
      c_optvallen : constant Interfaces.C.size_t := Option_Value'Length;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_setsockopt (Convert (S), Convert (Option), Convert (c_optval), c_optvallen);
      Interfaces.C.Strings.Free (c_optval);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Set_Sock_Opt;

   --  function zmq_getsockopt (s : System.Address; option : Interfaces.C.int; optval : System.Address; optvallen : access Interfaces.C.size_t) return Interfaces.C.int;

   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return Integer
   is
      c_optval : aliased Interfaces.C.int;
      c_optval_ptr : constant System.Address := c_optval'Address;
      c_optvallen : aliased Interfaces.C.size_t := c_optval'Size / 8;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_getsockopt (Convert (S), Convert (Option), c_optval_ptr, c_optvallen'Access);
      if c_res = 0 then
         return Integer (c_optval);
      else
         Raise_Errno;
      end if;
   end Get_Sock_Opt;

   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return Long_Long_Integer
   is
      c_optval : aliased Interfaces.C.long;
      c_optval_ptr : constant System.Address := c_optval'Address;
      c_optvallen : aliased Interfaces.C.size_t := c_optval'Size / 8;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_getsockopt (Convert (S), Convert (Option), c_optval_ptr, c_optvallen'Access);
      if c_res = 0 then
         return Long_Long_Integer (c_optval);
      else
         Raise_Errno;
      end if;
   end Get_Sock_Opt;

   function Get_Sock_Opt (S : Socket_Type; Option : Socket_Option_Names) return String
   is
      Buf_Size : constant Integer := 4096;
      c_optval : aliased String (1.. Buf_Size);
      c_optval_ptr : constant System.Address := c_optval'Address;
      c_optvallen : aliased Interfaces.C.size_t := c_optval'Length;

      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_getsockopt (Convert (S), Convert (Option), c_optval_ptr, c_optvallen'Access);
      if c_optvallen >= Interfaces.C.size_t (Buf_Size) then
         raise ZMQ_Error with "string option too large.";
      end if;
      if c_res = 0 then
         return c_optval (1 .. Integer (c_optvallen));
      else
         Raise_Errno;
      end if;
   end Get_Sock_Opt;



   procedure Bind (S : Socket_Type; Address : String)
   is
      c_addr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Address);
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_bind (Convert (S), c_addr);
      Interfaces.C.Strings.Free (c_addr);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Bind;

   procedure Connect (S : Socket_Type; Address : String)
   is
      c_addr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Address);
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_connect (Convert (S), c_addr);
      Interfaces.C.Strings.Free (c_addr);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Connect;

   procedure Unbind (S : Socket_Type; Address : String)
   is
      c_addr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Address);
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_unbind (Convert (S), c_addr);
      Interfaces.C.Strings.Free (c_addr);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Unbind;

   procedure Disconnect (S : Socket_Type; Address : String)
   is
      c_addr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Address);
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_disconnect (Convert (S), c_addr);
      Interfaces.C.Strings.Free (c_addr);
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Disconnect;

   --  TODO: What about an array of flags?
   procedure Send (S : Socket_Type; Buf : String; Do_Not_Wait : Boolean := False; Send_More : Boolean := False)
   is
      c_buf : constant System.Address := Buf'Address;  -- TODO is this okay?  or ptr to first item better?
      c_len : constant Interfaces.C.size_t := Buf'Length;
      c_flags : Interfaces.C.unsigned := 0;

      c_res : Interfaces.C.int;
   begin
      c_flags := 0;
      if Do_Not_Wait then
         c_flags := c_flags or ZMQ_Constants.ZMQ_DONTWAIT;
      end if;
      if Send_More then
         c_flags := c_flags or ZMQ_Constants.ZMQ_SNDMORE;
      end if;

      c_res := ZMQ_Thin.zmq_send (Convert (S), c_buf, c_len, c_flags);
      if c_res >= 0 and then Interfaces.C.size_t (c_res) = c_len then
         return;
      elsif c_res >= 0 then
         raise ZMQ_Error with "Entire Message_Type wasn't sent.";
      else
         Raise_Errno;
      end if;
   end Send;

   function Recv (S : Socket_Type; Do_Not_Wait : Boolean := False) return String
   is
      Buf_Size : constant Integer := 32*1024;
      Buf : String (1 .. Buf_Size);

      c_buf : constant System.Address := Buf (Buf'First)'Address;
      c_len : constant Interfaces.C.size_t := Buf'Length;
      c_flags : Interfaces.C.unsigned := 0;

      c_res : Interfaces.C.int;
   begin
      c_flags := 0;
      if Do_Not_Wait then
         c_flags := c_flags or ZMQ_Constants.ZMQ_DONTWAIT;
      end if;

      c_res := ZMQ_Thin.zmq_recv (Convert (S), c_buf, c_len, c_flags);
      if c_res >= 0 and then c_res /= Interfaces.C.int (Buf_Size) then
         return Buf (1 .. Integer (c_res));
      elsif c_res >= 0 then
         raise ZMQ_Error with "Received Message_Type was truncated.";
      else
         Raise_Errno;
      end if;
   end Recv;



   --  Polling
   --  ------------------------------------------------------------------------

   procedure Setup (PE : in out Poll_Item_Type; S : Socket_Type'Class; Poll_In : Boolean := False; Poll_Out : Boolean := False)
   is
   begin
      PE.S_Impl_Ptr := S.Impl_Ptr;
      PE.FD := 0;
      PE.Poll_In := Poll_In;
      PE.Poll_Out := Poll_Out;
      PE.Poll_Error := False;
      PE.Result_In := False;
      PE.Result_Out := False;
      PE.Result_Error := False;
   end Setup;

   function New_Poll_Item (S : Socket_Type'Class; Poll_In : Boolean := False; Poll_Out : Boolean := False) return Poll_Item_Type
   is
   begin
      return PE : Poll_Item_Type do
         Setup (PE, S, Poll_In, Poll_Out);
      end return;
   end New_Poll_Item;

   procedure Setup (PE : in out Poll_Item_Type; FD : Integer; Poll_In : Boolean := False; Poll_Out : Boolean := False; Poll_Error : Boolean := False)
   is
   begin
      PE.S_Impl_Ptr := System.Null_Address;
      PE.FD := Interfaces.C.int (FD);
      PE.Poll_In := Poll_In;
      PE.Poll_Out := Poll_Out;
      PE.Poll_Error := Poll_Error;
      PE.Result_In := False;
      PE.Result_Out := False;
      PE.Result_Error := False;
   end Setup;
   function New_Poll_Item (FD : Integer; Poll_In : Boolean := False; Poll_Out : Boolean := False; Poll_Error : Boolean := False) return Poll_Item_Type
   is
   begin
      return PE : Poll_Item_Type do
         Setup (PE, FD, Poll_In, Poll_Out, Poll_Error);
      end return;
   end New_Poll_Item;

   function Is_Readable (PE : Poll_Item_Type) return Boolean is (PE.Result_In);
   function Is_Writable (PE : Poll_Item_Type) return Boolean is (PE.Result_Out);
   function Has_Error (PE : Poll_Item_Type) return Boolean is (PE.Result_Error);


   type C_Poll_Item_Array is array (Natural range <>) of aliased ZMQ_Thin.zmq_pollitem_t;
   pragma Convention (C, C_Poll_Item_Array);

   function Convert (PE : Poll_Item_Type) return ZMQ_Thin.zmq_pollitem_t
   is
   begin
      return Result : ZMQ_Thin.zmq_pollitem_t do
         Result.socket := PE.S_Impl_Ptr;
         Result.fd := PE.FD;
         Result.events := 0;
         if PE.Poll_In then
            Result.events := Interfaces.C.short (Interfaces.C.unsigned (Result.events) or ZMQ_Constants.ZMQ_POLLIN);
         end if;
         if PE.Poll_Out then
            Result.events := Interfaces.C.short (Interfaces.C.unsigned (Result.events) or ZMQ_Constants.ZMQ_POLLOUT);
         end if;
         if PE.Poll_Error then
            Result.events := Interfaces.C.short (Interfaces.C.unsigned (Result.events) or ZMQ_Constants.ZMQ_POLLERR);
         end if;
      end return;
   end Convert;

   procedure Transfer_Result (Dest : in out Poll_Item_Type; Src : ZMQ_Thin.zmq_pollitem_t)
   is
   begin
      Dest.Result_In := (Interfaces.C.unsigned(Src.revents) and ZMQ_Constants.ZMQ_POLLIN) /= 0;
      Dest.Result_Out := (Interfaces.C.unsigned(Src.revents) and ZMQ_Constants.ZMQ_POLLOUT) /= 0;
   end;

   function Poll (Entries : in out Poll_Item_Array_Type; Timeout : Long_Long_Integer := -1) return Natural
   is
      c_items : aliased C_Poll_Item_Array (Entries'First .. Entries'Last);
      c_items_ptr : constant access ZMQ_Thin.zmq_pollitem_t := c_items (c_items'First)'Access;
      c_nitems : constant Interfaces.C.int := Interfaces.C.int (Entries'Length);
      c_timeout : constant Interfaces.C.long := Interfaces.C.long (Timeout);

      c_res : Interfaces.C.int;
   begin
      for I in Entries'Range loop
         c_items (I) := Convert (Entries (I));
      end loop;

      c_res := ZMQ_Thin.zmq_poll (c_items_ptr, c_nitems, c_timeout);

      if c_res < 0 then
         Raise_Errno;
      end if;

      for I in Entries'Range loop
         Transfer_Result (Entries (I), c_items (I));
      end loop;

      return Natural (c_res);
   end Poll;

   procedure Poll (Entries : in out Poll_Item_Array_Type; Timeout : Long_Long_Integer := -1)
   is
      Dummy : Natural;
   begin
      Dummy := Poll (Entries, Timeout);
   end Poll;


   --  Proxy
   --  ------------------------------------------------------------------------

   --  TODO, merge these?  using the optional parameters?

   procedure Proxy (Frontend : Socket_Type'Class; Backend : Socket_Type'Class; Capture : Socket_Type'Class := ZMQ.No_Socket)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_proxy (Convert (Frontend), Convert (Backend), Convert (Capture));
      --  proxy is expected to always return an error.
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Proxy;

   procedure Proxy_Steerable (Frontend : Socket_Type'Class; Backend : Socket_Type'Class; Capture : Socket_Type'Class := ZMQ.No_Socket; Control : Socket_Type'Class := ZMQ.No_Socket)
   is
      c_res : Interfaces.C.int;
   begin
      c_res := ZMQ_Thin.zmq_proxy_steerable (Convert (Frontend), Convert (Backend), Convert (Capture), Convert (Control));
      --  proxy_steerable can terminate cleanly
      if c_res = 0 then
         return;
      else
         Raise_Errno;
      end if;
   end Proxy_Steerable;

end ZMQ;
