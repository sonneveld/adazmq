with Interfaces.C;

package ZMQ_Constants is

   --  Errors
   ENOTSUP : constant Interfaces.C.int := 45;
   EPROTONOSUPPORT : constant Interfaces.C.int := 43;
   ENOBUFS : constant Interfaces.C.int := 55;
   ENETDOWN : constant Interfaces.C.int := 50;
   EADDRINUSE : constant Interfaces.C.int := 48;
   EADDRNOTAVAIL : constant Interfaces.C.int := 49;
   ECONNREFUSED : constant Interfaces.C.int := 61;
   EINPROGRESS : constant Interfaces.C.int := 36;
   ENOTSOCK : constant Interfaces.C.int := 38;
   EMSGSIZE : constant Interfaces.C.int := 40;
   EAFNOSUPPORT : constant Interfaces.C.int := 47;
   ENETUNREACH : constant Interfaces.C.int := 51;
   ECONNABORTED : constant Interfaces.C.int := 53;
   ECONNRESET : constant Interfaces.C.int := 54;
   ENOTCONN : constant Interfaces.C.int := 57;
   ETIMEDOUT : constant Interfaces.C.int := 60;
   EHOSTUNREACH : constant Interfaces.C.int := 65;
   ENETRESET : constant Interfaces.C.int := 52;
   EFSM : constant Interfaces.C.int := 156384763;
   ENOCOMPATPROTO : constant Interfaces.C.int := 156384764;
   ETERM : constant Interfaces.C.int := 156384765;
   EMTHREAD : constant Interfaces.C.int := 156384766;

   --  Context
   ZMQ_IO_THREADS : constant Interfaces.C.int := 1;
   ZMQ_MAX_SOCKETS : constant Interfaces.C.int := 2;
   ZMQ_IO_THREADS_DFLT : constant Interfaces.C.int := 1;
   ZMQ_MAX_SOCKETS_DFLT : constant Interfaces.C.int := 1023;

   --  Socket Types
   ZMQ_PAIR : constant Interfaces.C.int := 0;
   ZMQ_PUB : constant Interfaces.C.int := 1;
   ZMQ_SUB : constant Interfaces.C.int := 2;
   ZMQ_REQ : constant Interfaces.C.int := 3;
   ZMQ_REP : constant Interfaces.C.int := 4;
   ZMQ_DEALER : constant Interfaces.C.int := 5;
   ZMQ_ROUTER : constant Interfaces.C.int := 6;
   ZMQ_PULL : constant Interfaces.C.int := 7;
   ZMQ_PUSH : constant Interfaces.C.int := 8;
   ZMQ_XPUB : constant Interfaces.C.int := 9;
   ZMQ_XSUB : constant Interfaces.C.int := 10;
   ZMQ_STREAM : constant Interfaces.C.int := 11;

   --  Socket Options
   ZMQ_AFFINITY : constant Interfaces.C.int := 4;
   ZMQ_IDENTITY : constant Interfaces.C.int := 5;
   ZMQ_SUBSCRIBE : constant Interfaces.C.int := 6;
   ZMQ_UNSUBSCRIBE : constant Interfaces.C.int := 7;
   ZMQ_RATE : constant Interfaces.C.int := 8;
   ZMQ_RECOVERY_IVL : constant Interfaces.C.int := 9;
   ZMQ_SNDBUF : constant Interfaces.C.int := 11;
   ZMQ_RCVBUF : constant Interfaces.C.int := 12;
   ZMQ_RCVMORE : constant Interfaces.C.int := 13;
   ZMQ_FD : constant Interfaces.C.int := 14;
   ZMQ_EVENTS : constant Interfaces.C.int := 15;
   ZMQ_TYPE : constant Interfaces.C.int := 16;
   ZMQ_LINGER : constant Interfaces.C.int := 17;
   ZMQ_RECONNECT_IVL : constant Interfaces.C.int := 18;
   ZMQ_BACKLOG : constant Interfaces.C.int := 19;
   ZMQ_RECONNECT_IVL_MAX : constant Interfaces.C.int := 21;
   ZMQ_MAXMSGSIZE : constant Interfaces.C.int := 22;
   ZMQ_SNDHWM : constant Interfaces.C.int := 23;
   ZMQ_RCVHWM : constant Interfaces.C.int := 24;
   ZMQ_MULTICAST_HOPS : constant Interfaces.C.int := 25;
   ZMQ_RCVTIMEO : constant Interfaces.C.int := 27;
   ZMQ_SNDTIMEO : constant Interfaces.C.int := 28;
   ZMQ_LAST_ENDPOINT : constant Interfaces.C.int := 32;
   ZMQ_ROUTER_MANDATORY : constant Interfaces.C.int := 33;
   ZMQ_TCP_KEEPALIVE : constant Interfaces.C.int := 34;
   ZMQ_TCP_KEEPALIVE_CNT : constant Interfaces.C.int := 35;
   ZMQ_TCP_KEEPALIVE_IDLE : constant Interfaces.C.int := 36;
   ZMQ_TCP_KEEPALIVE_INTVL : constant Interfaces.C.int := 37;
   ZMQ_TCP_ACCEPT_FILTER : constant Interfaces.C.int := 38;
   ZMQ_IMMEDIATE : constant Interfaces.C.int := 39;
   ZMQ_XPUB_VERBOSE : constant Interfaces.C.int := 40;
   ZMQ_ROUTER_RAW : constant Interfaces.C.int := 41;
   ZMQ_IPV6 : constant Interfaces.C.int := 42;
   ZMQ_MECHANISM : constant Interfaces.C.int := 43;
   ZMQ_PLAIN_SERVER : constant Interfaces.C.int := 44;
   ZMQ_PLAIN_USERNAME : constant Interfaces.C.int := 45;
   ZMQ_PLAIN_PASSWORD : constant Interfaces.C.int := 46;
   ZMQ_CURVE_SERVER : constant Interfaces.C.int := 47;
   ZMQ_CURVE_PUBLICKEY : constant Interfaces.C.int := 48;
   ZMQ_CURVE_SECRETKEY : constant Interfaces.C.int := 49;
   ZMQ_CURVE_SERVERKEY : constant Interfaces.C.int := 50;
   ZMQ_PROBE_ROUTER : constant Interfaces.C.int := 51;
   ZMQ_REQ_CORRELATE : constant Interfaces.C.int := 52;
   ZMQ_REQ_RELAXED : constant Interfaces.C.int := 53;
   ZMQ_CONFLATE : constant Interfaces.C.int := 54;
   ZMQ_ZAP_DOMAIN : constant Interfaces.C.int := 55;

   --  Message Options
   ZMQ_MORE : constant Interfaces.C.int := 1;
   ZMQ_SRCFD : constant Interfaces.C.int := 2;
   ZMQ_SHARED : constant Interfaces.C.int := 3;

   --  Send/Recv Flags
   ZMQ_DONTWAIT : constant Interfaces.C.unsigned := 1;
   ZMQ_SNDMORE : constant Interfaces.C.unsigned := 2;

   --  Security Options
   ZMQ_NULL : constant Interfaces.C.int := 0;
   ZMQ_PLAIN : constant Interfaces.C.int := 1;
   ZMQ_CURVE : constant Interfaces.C.int := 2;

   --  Socket Event Flags
   ZMQ_EVENT_CONNECTED : constant Interfaces.C.unsigned := 1;
   ZMQ_EVENT_CONNECT_DELAYED : constant Interfaces.C.unsigned := 2;
   ZMQ_EVENT_CONNECT_RETRIED : constant Interfaces.C.unsigned := 4;
   ZMQ_EVENT_LISTENING : constant Interfaces.C.unsigned := 8;
   ZMQ_EVENT_BIND_FAILED : constant Interfaces.C.unsigned := 16;
   ZMQ_EVENT_ACCEPTED : constant Interfaces.C.unsigned := 32;
   ZMQ_EVENT_ACCEPT_FAILED : constant Interfaces.C.unsigned := 64;
   ZMQ_EVENT_CLOSED : constant Interfaces.C.unsigned := 128;
   ZMQ_EVENT_CLOSE_FAILED : constant Interfaces.C.unsigned := 256;
   ZMQ_EVENT_DISCONNECTED : constant Interfaces.C.unsigned := 512;
   ZMQ_EVENT_MONITOR_STOPPED : constant Interfaces.C.unsigned := 1024;
   ZMQ_EVENT_ALL : constant Interfaces.C.unsigned := 2047;

   --  I/O Multiplexing
   ZMQ_POLLIN : constant Interfaces.C.unsigned := 1;
   ZMQ_POLLOUT : constant Interfaces.C.unsigned := 2;
   ZMQ_POLLERR : constant Interfaces.C.unsigned := 4;

end ZMQ_Constants;
