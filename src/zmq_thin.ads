with Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ_Thin is

   --  Version

   procedure zmq_version (major : access Interfaces.C.int; minor : access Interfaces.C.int; patch : access Interfaces.C.int);
   pragma Import (C, zmq_version, "zmq_version");


   --  Errors

   function zmq_errno return Interfaces.C.int;
   pragma Import (C, zmq_errno, "zmq_errno");

   function zmq_strerror (errnum : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, zmq_strerror, "zmq_strerror");


   --  Contexts

   function zmq_ctx_new return System.Address;
   pragma Import (C, zmq_ctx_new, "zmq_ctx_new");

   function zmq_ctx_term (context : System.Address) return Interfaces.C.int;
   pragma Import (C, zmq_ctx_term, "zmq_ctx_term");

   function zmq_ctx_shutdown (ctx_u : System.Address) return Interfaces.C.int;
   pragma Import (C, zmq_ctx_shutdown, "zmq_ctx_shutdown");

   function zmq_ctx_set (context : System.Address; option : Interfaces.C.int; optval : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, zmq_ctx_set, "zmq_ctx_set");

   function zmq_ctx_get (context : System.Address; option : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, zmq_ctx_get, "zmq_ctx_get");


   --  Message

   type zmq_msg_t_u_u_array is array (1 .. 64) of Interfaces.C.unsigned_char;
   type zmq_msg_t is record
      u_u : zmq_msg_t_u_u_array := (others => 0);
   end record;
   for zmq_msg_t'Alignment use 8;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);

   function zmq_msg_init (msg : access zmq_msg_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_send (msg : access zmq_msg_t; s : System.Address; flags : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, zmq_msg_send, "zmq_msg_send");

   function zmq_msg_recv (msg : access zmq_msg_t; s : System.Address; flags : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, zmq_msg_recv, "zmq_msg_recv");

   function zmq_msg_close (msg : access zmq_msg_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return Interfaces.C.size_t;
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_msg_more (msg : access zmq_msg_t) return Interfaces.C.int;
   pragma Import (C, zmq_msg_more, "zmq_msg_more");

   function zmq_msg_get (msg : access zmq_msg_t; option : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, zmq_msg_get, "zmq_msg_get");

   function zmq_msg_set (msg : access zmq_msg_t; option : Interfaces.C.int; optval : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, zmq_msg_set, "zmq_msg_set");


   --  Sockets

   function zmq_socket (cxt : System.Address; s_type : Interfaces.C.int) return System.Address;
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return Interfaces.C.int;
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt (s : System.Address; option : Interfaces.C.int; optval : System.Address; optvallen : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt (s : System.Address; option : Interfaces.C.int; optval : System.Address; optvallen : access Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_unbind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, zmq_unbind, "zmq_unbind");

   function zmq_disconnect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, zmq_disconnect, "zmq_disconnect");

   function zmq_send (s : System.Address; buf : System.Address; len : Interfaces.C.size_t; flags : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_recv (s : System.Address; buf : System.Address; len : Interfaces.C.size_t; flags : Interfaces.C.unsigned) return Interfaces.C.int;
   pragma Import (C, zmq_recv, "zmq_recv");


   --  Polling

   type zmq_pollitem_t is record
      socket : System.Address := System.Null_Address;
      fd : Interfaces.C.int := 0;
      events : Interfaces.C.short := 0;
      revents : aliased Interfaces.C.short := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);

   function zmq_poll (items : access zmq_pollitem_t; nitems : Interfaces.C.int; timeout : Interfaces.C.long) return Interfaces.C.int;
   pragma Import (C, zmq_poll, "zmq_poll");


   --  Proxy

   function zmq_proxy (frontend : System.Address; backend : System.Address; capture : System.Address) return Interfaces.C.int;
   pragma Import (C, zmq_proxy, "zmq_proxy");

   function zmq_proxy_steerable (frontend : System.Address; backend : System.Address; capture : System.Address; control : System.Address) return Interfaces.C.int;
   pragma Import (C, zmq_proxy_steerable, "zmq_proxy_steerable");

end ZMQ_Thin;
