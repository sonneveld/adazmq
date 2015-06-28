--  Load-balancing broker
--  Clients and workers are shown here in-process

with Ada.Command_Line;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Formatted_String;

with ZMQ;
with ZHelper;

use type GNAT.Formatted_String.Formatted_String;
use type Ada.Real_Time.Time_Span;

procedure LBBroker is

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type => String, "=" => "=");

   Nbr_Clients : constant := 10;
   Nbr_Workers : constant := 3;


   --  Basic request-reply client using REQ socket
   --  Because s_send and s_recv can't handle 0MQ binary identities, we
   --  set a printable text identity to allow routing.

   task type Client_Task_Type is
      entry Start (Id : out Ada.Strings.Unbounded.Unbounded_String);
   end Client_Task_Type;

   task body Client_Task_Type is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Client : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      Self_Id : constant String := ZHelper.Set_Id (Client); --  Set a printable identity.
   begin
      accept Start (Id : out Ada.Strings.Unbounded.Unbounded_String) do
         Id := Ada.Strings.Unbounded.To_Unbounded_String (Self_Id);
      end Start;

      Client.Connect ("ipc://frontend.ipc");
      --  Send request, get reply
      Client.Send ("HELLO");
      declare
         Reply : constant String := Client.Recv;
      begin
         Ada.Text_IO.Put_Line ("Client: " & Reply);
      end;

      Client.Close;
      Context.Term;
   end Client_Task_Type;


   --  While this example runs in a single process, that is just to make
   --  it easier to start and stop the example. Each thread has its own
   --  context and conceptually acts as a separate process.
   --  This is the worker task, using a REQ socket to do load-balancing.
   --  Because s_send and s_recv can't handle 0MQ binary identities, we
   --  set a printable text identity to allow routing.

   task type Worker_Task_Type is
      entry Start (Id : out Ada.Strings.Unbounded.Unbounded_String);
   end Worker_Task_Type;

   task body Worker_Task_Type is
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Worker : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_REQ);
      Self_Id : constant String := ZHelper.Set_Id (Worker); --  Set a printable identity.
   begin
      accept Start (Id : out Ada.Strings.Unbounded.Unbounded_String) do
         Id := Ada.Strings.Unbounded.To_Unbounded_String (Self_Id);
      end Start;

      Worker.Connect ("ipc://backend.ipc");

      --  Tell broker we're ready for work
      Worker.Send ("READY");

      Read_Loop :
      loop
         --  Read and save all frames until we get an empty frame
         --  In this example there is only 1, but there could be more
         declare
            Identity : constant String := Worker.Recv;
            Empty : constant String := Worker.Recv;
            Request : constant String := Worker.Recv;
         begin
            if Empty'Length /= 0 then
               raise Program_Error;
            end if;

            exit Read_Loop when Request = "TERMINATE";

            --  Get request, send reply
            Ada.Text_IO.Put_Line ("Worker: " & Request);

            Worker.Send (Identity, Send_More => True);
            Worker.Send ("", Send_More => True);
            Worker.Send ("OK");
         end;
      end loop Read_Loop;

      Worker.Close;
      Context.Term;
   end Worker_Task_Type;


   --  This is the main task. It starts the clients and workers, and then
   --  routes requests between the two layers. Workers signal READY when
   --  they start; after that we treat them as ready when they reply with
   --  a response back to a client. The load-balancing data structure is
   --  just a queue of next available workers.

   function Main return Ada.Command_Line.Exit_Status
   is
      --  Prepare our context and sockets
      Context : ZMQ.Context_Type := ZMQ.New_Context;
      Frontend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
      Backend : ZMQ.Socket_Type'Class := Context.New_Socket (ZMQ.ZMQ_ROUTER);
   begin
      Frontend.Bind ("ipc://frontend.ipc");
      Backend.Bind ("ipc://backend.ipc");

      declare
         Client_Tasks : array (1 .. Nbr_Clients) of Client_Task_Type;
         Worker_Tasks : array (1 .. Nbr_Workers) of Worker_Task_Type;
         Worker_Ids : String_Lists.List;
         Client_Reply_Count : Natural := 0;
         T_Id : Ada.Strings.Unbounded.Unbounded_String;
      begin
         for T of Client_Tasks loop
            T.Start (T_Id);
         end loop;
         for T of Worker_Tasks loop
            T.Start (T_Id);
            Worker_Ids.Append (Ada.Strings.Unbounded.To_String (T_Id));
         end loop;

         --  Here is the main loop for the least-recently-used queue. It has two
         --  sockets; a frontend for clients and a backend for workers. It polls
         --  the backend in all cases, and polls the frontend only when there are
         --  one or more workers ready. This is a neat way to use 0MQ's own queues
         --  to hold messages we're not ready to process yet. When we get a client
         --  reply, we pop the next available worker and send the request to it,
         --  including the originating client identity. When a worker replies, we
         --  requeue that worker and forward the reply to the original client
         --  using the reply envelope.

         --  Queue of available workers
         declare
            Worker_Queue : String_Lists.List;
         begin

            Poll_Loop :
            loop
               declare
                  Items : ZMQ.Poll_Item_Array_Type :=
                     (ZMQ.New_Poll_Item (Backend, Poll_In => True),
                     ZMQ.New_Poll_Item (Frontend, Poll_In => True));
               begin
                  --  Poll frontend only if we have available workers
                  if Worker_Queue.Is_Empty then
                     ZMQ.Poll (Items (Items'First .. Items'First));
                  else
                     ZMQ.Poll (Items);
                  end if;

                  --  Handle worker activity on backend
                  if ZMQ.Is_Readable (Items (Items'First)) then
                     --  Queue worker identity for load-balancing
                     declare
                        Worker_Id : constant String := Backend.Recv;
                        Empty : constant String := Backend.Recv;  --  Second frame is empty
                        Client_Id : constant String := Backend.Recv;  --  Third frame is READY or else a client reply identity
                     begin
                        Worker_Queue.Append (Worker_Id);
                        if Empty'Length /= 0 then
                           raise Program_Error;
                        end if;
                        --  If client reply, send rest back to frontend
                        if Client_Id /= "READY" then
                           declare
                              Empty : constant String := Backend.Recv;
                              Reply : constant String := Backend.Recv;
                           begin
                              if Empty'Length /= 0 then
                                 raise Program_Error;
                              end if;
                              Frontend.Send (Client_Id, Send_More => True);
                              Frontend.Send ("", Send_More => True);
                              Frontend.Send (Reply);
                              Client_Reply_Count := Client_Reply_Count + 1;
                           end;
                        end if;
                     end;
                  end if;

                  --  Here is how we handle a client request:
                  if not Worker_Queue.Is_Empty and then ZMQ.Is_Readable (Items (Items'First + 1)) then
                     --  Now get next client request, route to last-used worker
                     --  Client request is [identity][empty][request]
                     declare
                        Client_Id : constant String := Frontend.Recv;
                        Empty : constant String := Frontend.Recv;
                        Request : constant String := Frontend.Recv;

                        Worker_Id : constant String := Worker_Queue.First_Element;
                     begin
                        if Empty'Length /= 0 then
                           raise Program_Error;
                        end if;

                        Backend.Send (Worker_Id, Send_More => True);
                        Backend.Send ("", Send_More => True);
                        Backend.Send (Client_Id, Send_More => True);
                        Backend.Send ("", Send_More => True);
                        Backend.Send (Request);

                        --  Dequeue and drop the next worker identity
                        Worker_Queue.Delete_First;
                     end;

                  end if;
               end;

               exit Poll_Loop when Client_Reply_Count >= Nbr_Clients;  --  Exit after N messages

            end loop Poll_Loop;
         end;

         for X of Worker_Ids loop
            Backend.Send (X, Send_More => True);
            Backend.Send ("", Send_More => True);
            Backend.Send ("<none>", Send_More => True);
            Backend.Send ("", Send_More => True);
            Backend.Send ("TERMINATE");
         end loop;

         Frontend.Close;
         Backend.Close;
         Context.Term;
      end;

      return 0;
   end Main;

begin
   Ada.Command_Line.Set_Exit_Status (Main);
end LBBroker;
