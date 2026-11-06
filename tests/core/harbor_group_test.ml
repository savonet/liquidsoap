(* Delivery groups under a stop forced between a pass's flush and its
   removal: the stop waits for the pass, every socket is closed exactly once,
   every teardown runs exactly once, and a publication after the stop is
   refused with the socket left to its caller. *)

module Group = Harbor_output.Group
module Listener = Harbor_output.Listener

let log = Log.make ["test"]

type probe = {
  listener : Listener.t;
  closes : int Atomic.t;
  teardowns : int Atomic.t;
}

let socket closes =
  object
    method typ = "fake"
    method transport = Liq_http.unix_transport
    method file_descr = Unix.stdin
    method wait_for ?log:_ _ _ = ()
    method pending = false
    method non_blocking _ = ()
    method write _ _ length = length
    method read _ _ _ = 0
    method closed = Atomic.get closes > 0
    method close = Atomic.incr closes
  end

let probe ?deadline ~group id =
  let closes = Atomic.make 0 in
  let info =
    {
      Listener.id;
      ip = "127.0.0.1";
      peer = Printf.sprintf "127.0.0.1:%d" id;
      uri = "/test";
      query_string = "";
      protocol = "1.0";
      headers = [];
      connected_at = Unix.gettimeofday ();
    }
  in
  let listener =
    Listener.create ?deadline ~log
      ~wake:(fun () -> Group.wake group)
      info (socket closes)
  in
  { listener; closes; teardowns = Atomic.make 0 }

let rec wait_until ?(tries = 200) label cond =
  if not (cond ()) then (
    if tries = 0 then failwith ("timeout waiting for " ^ label);
    Unix.sleepf 0.01;
    wait_until ~tries:(tries - 1) label cond)

let stop_between_flush_and_removal () =
  let group = Group.create ~log ~timeout:30. () in
  let in_flush = Atomic.make false and release = Atomic.make false in
  let entered = Atomic.make false in
  let blocked = probe ~group 0 and other = probe ~group 1 in
  let member ?(block = false) probe =
    {
      Group.listener = probe.listener;
      pending = (fun () -> not (Listener.is_closed probe.listener));
      flush =
        (fun ~scratch:_ ->
          if block && Atomic.compare_and_set entered false true then (
            Atomic.set in_flush true;
            while not (Atomic.get release) do
              Unix.sleepf 0.005
            done;
            Listener.mark_closed probe.listener));
      teardown = (fun () -> Atomic.incr probe.teardowns);
    }
  in
  assert (Group.publish group (member ~block:true blocked));
  assert (Group.publish group (member other));
  wait_until "flush" (fun () -> Atomic.get in_flush);
  let stopped = Atomic.make false in
  let stopper =
    Thread.create
      (fun () ->
        Group.stop group;
        Atomic.set stopped true)
      ()
  in
  Unix.sleepf 0.2;
  assert (not (Atomic.get stopped));
  Atomic.set release true;
  Thread.join stopper;
  wait_until "teardowns" (fun () ->
      Atomic.get blocked.teardowns >= 1 && Atomic.get other.teardowns >= 1);
  Unix.sleepf 0.2;
  List.iter
    (fun p ->
      assert (Listener.is_closed p.listener);
      assert (Atomic.get p.closes = 1);
      assert (Atomic.get p.teardowns = 1))
    [blocked; other];
  let late = probe ~group 2 in
  assert (
    not
      (Group.publish group
         {
           Group.listener = late.listener;
           pending = (fun () -> true);
           flush = (fun ~scratch:_ -> ());
           teardown = (fun () -> Atomic.incr late.teardowns);
         }));
  assert (Atomic.get late.closes = 0);
  assert (not (Group.has_members group))

(* A flush that raises closes its own listener only, and later passes keep
   serving the others. *)
let raising_flush () =
  let group = Group.create ~log ~timeout:30. () in
  let faulty = probe ~group 10 and healthy = probe ~group 11 in
  let served = Atomic.make 0 in
  let member probe flush =
    {
      Group.listener = probe.listener;
      pending = (fun () -> not (Listener.is_closed probe.listener));
      flush;
      teardown = (fun () -> Atomic.incr probe.teardowns);
    }
  in
  assert (
    Group.publish group (member faulty (fun ~scratch:_ -> failwith "boom")));
  assert (
    Group.publish group (member healthy (fun ~scratch:_ -> Atomic.incr served)));
  wait_until "faulty closed" (fun () -> Atomic.get faulty.teardowns = 1);
  let before = Atomic.get served in
  Group.wake group;
  Group.wake group;
  wait_until "later passes" (fun () -> Atomic.get served > before);
  assert (not (Listener.is_closed healthy.listener));
  assert (Atomic.get faulty.closes = 1);
  Group.stop group;
  wait_until "healthy teardown" (fun () -> Atomic.get healthy.teardowns = 1)

(* A listener past its deadline is closed even when it is owed nothing, so
   that no data wakes its group. *)
let idle_deadline () =
  let group = Group.create ~log ~timeout:30. () in
  let idle = probe ~deadline:(Listener.now () +. 0.2) ~group 20 in
  assert (
    Group.publish group
      {
        Group.listener = idle.listener;
        pending = (fun () -> false);
        flush = (fun ~scratch:_ -> ());
        teardown = (fun () -> Atomic.incr idle.teardowns);
      });
  wait_until "deadline teardown" (fun () -> Atomic.get idle.teardowns = 1);
  assert (Atomic.get idle.closes = 1);
  Group.stop group

let () =
  Scheduler.start ();
  stop_between_flush_and_removal ();
  raising_flush ();
  idle_deadline ();
  print_endline "group checks passed";
  exit 0
