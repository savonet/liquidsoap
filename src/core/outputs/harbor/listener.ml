type info = {
  id : int;
  ip : string;
  peer : string;
  uri : string;
  protocol : string;
  headers : (string * string) list;
  connected_at : float;
}

type t = {
  info : info;
  log : Log.t;
  socket : Harbor.socket;
  wake : unit -> unit;
  bytes_sent : int Atomic.t;
  mutable last_progress : float;
  closed_at : float option Atomic.t;
}

let create ~log ~wake info socket =
  {
    info;
    log;
    socket;
    wake;
    bytes_sent = Atomic.make 0;
    last_progress = 0.;
    closed_at = Atomic.make None;
  }

let info t = t.info
let log t = t.log
let socket t = t.socket

let now () =
  let module Time = (val Clock.time_implementation ()) in
  Time.to_float (Time.time ())

let write t data offset length =
  match t.socket#write data offset length with
    | 0 -> 0
    | accepted ->
        t.last_progress <- now ();
        ignore (Atomic.fetch_and_add t.bytes_sent accepted);
        accepted
    | exception Unix.Unix_error (error, _, _) when Liq_http.would_block error ->
        0
    | exception
        Unix.Unix_error
          (((Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNABORTED) as e), _, _) ->
        t.log#info "Socket write for %s: %s (disconnecting)" t.info.peer
          (Unix.error_message e);
        -1
    | exception exn ->
        t.log#important "Socket write error for %s: %s" t.info.peer
          (Printexc.to_string exn);
        -1

let last_progress t = t.last_progress
let set_last_progress t time = t.last_progress <- time

let mark_closed t =
  if Atomic.compare_and_set t.closed_at None (Some (Unix.gettimeofday ())) then (
    t.log#info "Listener %s disconnected" t.info.peer;
    t.wake ())

let is_closed t = Atomic.get t.closed_at <> None
let close_socket t = try t.socket#close with _ -> ()

let record_t =
  Lang.record_t
    [
      ("id", Lang.int_t);
      ("ip", Lang.string_t);
      ("uri", Lang.string_t);
      ("protocol", Lang.string_t);
      ("headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
      ("connected_at", Lang.float_t);
      ("duration", Lang.float_t);
      ("bytes_sent", Lang.int_t);
    ]

let to_record info ~duration ~bytes_sent =
  Lang.record
    [
      ("id", Lang.int info.id);
      ("ip", Lang.string info.ip);
      ("uri", Lang.string info.uri);
      ("protocol", Lang.string info.protocol);
      ( "headers",
        Lang.list
          (List.map
             (fun (name, value) ->
               Lang.product (Lang.string name) (Lang.string value))
             info.headers) );
      ("connected_at", Lang.float info.connected_at);
      ("duration", Lang.float duration);
      ("bytes_sent", Lang.int bytes_sent);
    ]

let to_value t =
  let until =
    match Atomic.get t.closed_at with
      | Some time -> time
      | None -> Unix.gettimeofday ()
  in
  to_record t.info
    ~duration:(until -. t.info.connected_at)
    ~bytes_sent:(Atomic.get t.bytes_sent)

let info_to_value info =
  to_record info
    ~duration:(Unix.gettimeofday () -. info.connected_at)
    ~bytes_sent:0
