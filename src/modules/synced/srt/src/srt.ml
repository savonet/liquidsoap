open Ctypes
open Posix_socket
module Srt = Srt_stubs.Def (Srt_generated_stubs)
open Srt
module Srt_locked = Srt_stubs_locked.Def (Srt_generated_stubs_locked)
open Srt_locked
open Unsigned

exception Invalid_argument of string
exception Error of errno * string

type errno = Srt.errno
type transtype = Srt.transtype
type socket_status = Srt.socket_status
type socket = Srt.socket

let socket_id s = s
let resources = Hashtbl.create 0

let string_of_errno = function
  | `Easyncfail -> "Easyncfail"
  | `Easyncrcv -> "Easyncrcv"
  | `Easyncsnd -> "Easyncsnd"
  | `Eboundsock -> "Eboundsock"
  | `Econgest -> "Econgest"
  | `Econnfail -> "Econnfail"
  | `Econnlost -> "Econnlost"
  | `Econnrej -> "Econnrej"
  | `Econnsetup -> "Econnsetup"
  | `Econnsock -> "Econnsock"
  | `Eduplisten -> "Eduplisten"
  | `Efile -> "Efile"
  | `Einvalbufferapi -> "Einvalbufferapi"
  | `Einvalmsgapi -> "Einvalmsgapi"
  | `Einvop -> "Einvop"
  | `Einvparam -> "Einvparam"
  | `Einvpollid -> "Einvpollid"
  | `Einvrdoff -> "Einvrdoff"
  | `Einvsock -> "Einvsock"
  | `Einvwroff -> "Einvwroff"
  | `Elargemsg -> "Elargemsg"
  | `Enobuf -> "Enobuf"
  | `Enoconn -> "Enoconn"
  | `Enolisten -> "Enolisten"
  | `Enoserver -> "Enoserver"
  | `Epeererr -> "Epeererr"
  | `Epollempty -> "Epollempty"
  | `Erdperm -> "Erdperm"
  | `Erdvnoserv -> "Erdvnoserv"
  | `Erdvunbound -> "Erdvunbound"
  | `Eresource -> "Eresource"
  | `Esclosed -> "Esclosed"
  | `Esecfail -> "Esecfail"
  | `Esockfail -> "Esockfail"
  | `Esysobj -> "Esysobj"
  | `Ethread -> "Ethread"
  | `Etimeout -> "Etimeout"
  | `Eunboundsock -> "Eunboundsock"
  | `Eunknown -> "Eunknown"
  | `Ewrperm -> "Ewrperm"
  | `Success -> "Success"

let () =
  Printexc.register_printer (function
    | Error (errno, msg) ->
        Some (Printf.sprintf "Error(%s,%s)" (string_of_errno errno) msg)
    | _ -> None)

let check_err ret =
  if ret = -1 then begin
    match getlasterror (from_voidp Ctypes.int Ctypes.null) with
      | `Success -> assert false
      | errno ->
          let msg = getlasterror_str () in
          clearlasterror ();
          raise (Error (errno, msg))
  end;
  ret

type (_, _) socket_opt =
  | Messageapi : ([ `Write ], bool) socket_opt
  | Payloadsize : ([ `Write ], int) socket_opt
  | Transtype : ([ `Write ], transtype) socket_opt
  | Rcvsyn : ([ `Read | `Write ], bool) socket_opt
  | Sndsyn : ([ `Read | `Write ], bool) socket_opt
  | Conntimeo : ([ `Write ], int) socket_opt
  | Rcvtimeo : ([ `Read | `Write ], int) socket_opt
  | Sndtimeo : ([ `Read | `Write ], int) socket_opt
  | Reuseaddr : ([ `Read | `Write ], bool) socket_opt
  | Rcvbuf : ([ `Read | `Write ], int) socket_opt
  | Sndbuf : ([ `Read | `Write ], int) socket_opt
  | Udp_rcvbuf : ([ `Read | `Write ], int) socket_opt
  | Udp_sndbuf : ([ `Read | `Write ], int) socket_opt
  | Rcvdata : ([ `Read ], int) socket_opt
  | Rcvlatency : ([ `Read | `Write ], int) socket_opt
  | Enforced_encryption : ([ `Write ], bool) socket_opt
  | Streamid : ([ `Read | `Write ], string) socket_opt
  | Passphrase : ([ `Write ], string) socket_opt
  | Pbkeylen : ([ `Read | `Write ], int) socket_opt
  | Ipv6only : ([ `Read | `Write ], bool) socket_opt
  | Latency : ([ `Read | `Write ], int) socket_opt
  | Peerlatency : ([ `Read | `Write ], int) socket_opt

let messageapi = Messageapi
let payloadsize = Payloadsize
let transtype = Transtype
let rcvsyn = Rcvsyn
let sndsyn = Sndsyn
let conntimeo = Conntimeo
let rcvtimeo = Rcvtimeo
let sndtimeo = Sndtimeo
let reuseaddr = Reuseaddr
let rcvbuf = Rcvbuf
let sndbuf = Sndbuf
let udp_rcvbuf = Udp_rcvbuf
let udp_sndbuf = Udp_sndbuf
let rcvdata = Rcvdata
let rcvlatency = Rcvlatency
let enforced_encryption = Enforced_encryption
let passphrase = Passphrase
let pbkeylen = Pbkeylen
let streamid = Streamid
let ipv6only = Ipv6only
let latency = Latency
let peerlatency = Peerlatency

let srt_socket_opt_of_socket_opt (type a b) :
    (a, b) socket_opt -> Srt.socket_opt = function
  | Messageapi -> `Messageapi
  | Payloadsize -> `Payloadsize
  | Transtype -> `Transtype
  | Rcvsyn -> `Rcvsyn
  | Sndsyn -> `Sndsyn
  | Conntimeo -> `Conntimeo
  | Rcvtimeo -> `Rcvtimeo
  | Sndtimeo -> `Sndtimeo
  | Reuseaddr -> `Reuseaddr
  | Rcvbuf -> `Rcvbuf
  | Sndbuf -> `Sndbuf
  | Rcvdata -> `Rcvdata
  | Rcvlatency -> `Rcvlatency
  | Udp_rcvbuf -> `Udp_rcvbuf
  | Udp_sndbuf -> `Udp_sndbuf
  | Enforced_encryption -> `Enforced_encryption
  | Pbkeylen -> `Pbkeylen
  | Passphrase -> `Passphrase
  | Streamid -> `Streamid
  | Ipv6only -> `Ipv6only
  | Latency -> `Latency
  | Peerlatency -> `Peerlatency

let srtt_live = Int64.to_int srtt_live
let srtt_file = Int64.to_int srtt_file
let srtt_invalid = Int64.to_int srtt_invalid

let int_of_transtype = function
  | `Live -> srtt_live
  | `File -> srtt_file
  | `Invalid -> srtt_invalid

let transtype_of_int = function
  | x when x = srtt_live -> `Live
  | x when x = srtt_file -> `File
  | x when x = srtt_invalid -> `Invalid
  | x ->
      raise (Invalid_argument ("Invalid transtype value: " ^ string_of_int x))

open Ctypes

let apply_sockaddr fn sockaddr = fn sockaddr (sockaddr_len sockaddr)

let bind_posix_socket socket socketaddr =
  ignore (check_err (apply_sockaddr (bind socket) socketaddr))

let bind socket socketaddr =
  ignore
    (check_err (apply_sockaddr (bind socket) (from_unix_sockaddr socketaddr)))

let connect_posix_socket socket socketaddr =
  ignore (check_err (apply_sockaddr (connect socket) socketaddr))

let connect socket socketaddr =
  ignore
    (check_err
       (apply_sockaddr (connect socket) (from_unix_sockaddr socketaddr)))

let accept_no_origin socket =
  let sockaddr = from_voidp Sockaddr.t null in
  let socklen = allocate int 0 in
  check_err (accept socket sockaddr socklen)

let accept socket =
  let sockaddr = Sockaddr.from_sockaddr_storage (sockaddr_storage ()) in
  let socklen = allocate int sockaddr_storage_len in
  let socket = check_err (accept socket sockaddr socklen) in
  (socket, to_unix_sockaddr sockaddr)

let rendez_vous_posix_socket socket sockaddr1 sockaddr2 =
  ignore
    (check_err
       (apply_sockaddr
          (apply_sockaddr (rendez_vous socket) sockaddr1)
          sockaddr2))

let rendez_vous socket sockaddr1 sockaddr2 =
  ignore
    (check_err
       (apply_sockaddr
          (apply_sockaddr (rendez_vous socket) (from_unix_sockaddr sockaddr1))
          (from_unix_sockaddr sockaddr2)))

type listen_callback = socket -> int -> Unix.sockaddr -> string option -> bool

let listen_callback sock fn =
  let fn _ s hs_version peeraddr streamid =
    let streamid =
      if is_null streamid then None
      else Some (string_from_ptr streamid ~length:(strlen streamid))
    in
    match fn s hs_version (to_unix_sockaddr peeraddr) streamid with
      | true -> 0
      | false | (exception _) -> -1
  in
  let ptr = Srt_stubs.ListenCallback.of_fun fn in
  let cleanup () = Srt_stubs.ListenCallback.free ptr in
  (try ignore (check_err (listen_callback sock ptr null))
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     cleanup ();
     Printexc.raise_with_backtrace exn bt);
  Hashtbl.add resources sock cleanup

let listen sock backlog = ignore (check_err (listen sock backlog))

let send sock msg =
  let len = Bytes.length msg in
  let ptr = allocate_n char ~count:len in
  memcpy_str ptr (ocaml_string_start (Bytes.unsafe_to_string msg)) len;
  check_err (send sock ptr len)

let sendmsg sock msg b v =
  let len = Bytes.length msg in
  let ptr = allocate_n char ~count:len in
  memcpy_str ptr (ocaml_string_start (Bytes.unsafe_to_string msg)) len;
  check_err (sendmsg sock ptr len b v)

let mk_recv fn sock buf len =
  if Bytes.length buf < len then raise (Invalid_argument "buffer too short!");
  let ptr = allocate_n char ~count:len in
  let length = check_err (fn sock ptr len) in
  memcpy (ocaml_bytes_start buf) ptr length;
  length

let recv = mk_recv recv
let recvmsg = mk_recv recvmsg

let getsockflag : type a b. socket -> (a, b) socket_opt -> b =
 fun sock opt ->
  let arg = allocate int 0 in
  let arglen = allocate int (sizeof int) in
  ignore
    (check_err
       (getsockflag sock
          (srt_socket_opt_of_socket_opt opt)
          (to_voidp arg) arglen));
  let to_bool () = !@arg <> 0 in
  let to_int () = !@arg in
  let to_string () =
    let arg = coerce (ptr int) (ptr char) arg in
    string_from_ptr arg ~length:!@arglen
  in
  match opt with
    | Enforced_encryption -> to_bool ()
    | Rcvsyn -> to_bool ()
    | Sndsyn -> to_bool ()
    | Reuseaddr -> to_bool ()
    | Messageapi -> to_bool ()
    | Conntimeo -> to_int ()
    | Rcvtimeo -> to_int ()
    | Sndtimeo -> to_int ()
    | Rcvbuf -> to_int ()
    | Sndbuf -> to_int ()
    | Udp_rcvbuf -> to_int ()
    | Pbkeylen -> to_int ()
    | Udp_sndbuf -> to_int ()
    | Payloadsize -> to_int ()
    | Rcvdata -> to_int ()
    | Rcvlatency -> to_int ()
    | Transtype -> transtype_of_int !@arg
    | Passphrase -> to_string ()
    | Streamid -> to_string ()
    | Ipv6only -> to_bool ()
    | Latency -> to_int ()
    | Peerlatency -> to_int ()

let setsockflag : type a b. socket -> (a, b) socket_opt -> b -> unit =
 fun sock opt v ->
  let f t v = to_voidp (allocate t v) in
  let of_bool v =
    let v = if v then 1 else 0 in
    (f int v, sizeof int)
  in
  let of_int v = (f int v, sizeof int) in
  let of_string v =
    let len = String.length v in
    let ptr = allocate_n char ~count:len in
    memcpy_str ptr (ocaml_string_start v) len;
    (to_voidp ptr, len)
  in
  let arg, arglen =
    match opt with
      | Enforced_encryption -> of_bool v
      | Rcvsyn -> of_bool v
      | Sndsyn -> of_bool v
      | Reuseaddr -> of_bool v
      | Messageapi -> of_bool v
      | Conntimeo -> of_int v
      | Rcvtimeo -> of_int v
      | Sndtimeo -> of_int v
      | Rcvbuf -> of_int v
      | Sndbuf -> of_int v
      | Udp_rcvbuf -> of_int v
      | Pbkeylen -> of_int v
      | Udp_sndbuf -> of_int v
      | Payloadsize -> of_int v
      | Rcvdata -> of_int v
      | Rcvlatency -> of_int v
      | Transtype ->
          let transtype = int_of_transtype v in
          (f int transtype, sizeof int)
      | Passphrase -> of_string v
      | Streamid -> of_string v
      | Ipv6only -> of_bool v
      | Latency -> of_int v
      | Peerlatency -> of_int v
  in
  ignore
    (check_err (setsockflag sock (srt_socket_opt_of_socket_opt opt) arg arglen))

let close s =
  ignore (check_err (close s));
  List.iter (fun fn -> fn ()) (Hashtbl.find_all resources s);
  Hashtbl.remove resources s

let getsockstate = getsockstate
let create_socket = create_socket

module SocketSet = Set.Make (struct
  type t = Srt.socket

  let compare = Stdlib.compare
end)

module Poll = struct
  type t = { eid : int; mutable sockets : SocketSet.t }
  type flag = Srt.poll_flag
  type _event = { _fd : socket; mutable _events : flag list }
  type event = { fd : socket; events : flag list }

  let create () = { eid = epoll_create (); sockets = SocketSet.empty }
  let sockets h = SocketSet.elements h.sockets

  let add_usock ?flags h s =
    let flags =
      match flags with
        | None -> Ctypes.(from_voidp int null)
        | Some flags ->
            let flags =
              List.fold_left
                (fun cur flag -> cur lor Int64.to_int (poll_flag_of_flag flag))
                0 flags
            in
            allocate Ctypes.int flags
    in
    ignore (check_err (epoll_add_usock h.eid s flags));
    h.sockets <- SocketSet.add s h.sockets

  let remove_usock h s =
    ignore (check_err (epoll_remove_usock h.eid s));
    h.sockets <- SocketSet.remove s h.sockets

  let update_usock ?flags h s =
    let flags =
      match flags with
        | None -> Ctypes.(from_voidp int null)
        | Some flags ->
            let flags =
              List.fold_left
                (fun cur flag -> cur lor Int64.to_int (poll_flag_of_flag flag))
                0 flags
            in
            allocate Ctypes.int flags
    in
    ignore (check_err (epoll_update_usock h.eid s flags));
    h.sockets <- SocketSet.remove s h.sockets

  let release h = ignore (check_err (epoll_release h.eid))

  let uwait h ~timeout =
    let timeout = Int64.of_int timeout in
    let max_fds = 3 * SocketSet.cardinal h.sockets in
    let events = CArray.make PollEvent.t max_fds in
    let nb_events =
      check_err (epoll_uwait h.eid (CArray.start events) max_fds timeout)
    in
    let events = CArray.to_list (CArray.sub events ~pos:0 ~length:nb_events) in
    let events =
      List.fold_left
        (fun events event ->
          let _fd = getf event PollEvent.fd in
          let _events = getf event PollEvent.events in
          let _events =
            List.fold_left
              (fun cur flag ->
                let poll_flag = Int64.to_int (poll_flag_of_flag flag) in
                if poll_flag land _events <> 0 then flag :: cur else cur)
              [] [`Read; `Write; `Error]
          in
          match List.find_opt (fun s -> s._fd = _fd) events with
            | Some e ->
                e._events <- List.sort_uniq Stdlib.compare (_events @ e._events);
                events
            | None -> { _fd; _events } :: events)
        [] events
    in
    List.map (fun { _fd = fd; _events = events } -> { fd; events }) events

  let wait h ~timeout =
    let max_fds = SocketSet.cardinal h.sockets in
    let timeout = Int64.of_int timeout in
    let read_len = allocate int max_fds in
    let read = CArray.make int max_fds in
    let write_len = allocate int max_fds in
    let write = CArray.make int max_fds in
    ignore
      (check_err
         (epoll_wait h.eid (CArray.start read) read_len (CArray.start write)
            write_len timeout null null null null));
    let read =
      List.sort_uniq Stdlib.compare
        (CArray.to_list (CArray.sub read ~pos:0 ~length:!@read_len))
    in
    let write =
      List.sort_uniq Stdlib.compare
        (CArray.to_list (CArray.sub write ~pos:0 ~length:!@write_len))
    in
    (read, write)
end

module Log = struct
  type msg = {
    level : int;
    file : string;
    line : int;
    area : string;
    message : string;
  }

  type level = [ `Critical | `Error | `Warning | `Notice | `Debug ]

  let int_of_level = function
    | `Critical -> log_crit
    | `Error -> log_err
    | `Warning -> log_warning
    | `Notice -> log_notice
    | `Debug -> log_debug

  let setloglevel lvl = setloglevel (int_of_level lvl)

  external setup_log_callback : unit -> unit = "ocaml_srt_setup_log_callback"
  [@@noalloc]

  external process_log : (msg -> unit) -> unit = "ocaml_srt_process_log"

  let log_fn = ref (fun _ -> ())
  let log_fn_m = Mutex.create ()

  let mutexify fn x =
    Mutex.lock log_fn_m;
    try
      fn x;
      Mutex.unlock log_fn_m
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Mutex.unlock log_fn_m;
      Printexc.raise_with_backtrace exn bt

  let set_handler fn =
    setup_log_callback ();
    mutexify (fun () -> log_fn := fn) ()

  external clear_callback : unit -> unit = "ocaml_srt_clear_log_callback"
  [@@noalloc]

  let clear_handler () =
    clear_callback ();
    mutexify (fun () -> log_fn := fun _ -> ()) ()

  let processor_state = Atomic.make `Idle

  let start_thread () =
    ignore
      (Thread.create
         (fun () ->
           process_log (fun msg ->
               if Atomic.compare_and_set processor_state `Stopping `Idle then ()
               else !log_fn msg))
         ())

  let rec startup () =
    if Atomic.get processor_state = `Running then ()
    else if Atomic.compare_and_set processor_state `Idle `Running then
      start_thread ()
    else if Atomic.compare_and_set processor_state `Stopping `Running then ()
    else startup ()

  let rec cleanup () =
    if List.mem (Atomic.get processor_state) [`Idle; `Stopping] then ()
    else if Atomic.compare_and_set processor_state `Running `Stopping then ()
    else cleanup ()
end

module Stats = struct
  type t = {
    msTimeStamp : int64;
    pktSentTotal : int64;
    pktRecvTotal : int64;
    pktSndLossTotal : int;
    pktRcvLossTotal : int;
    pktRetransTotal : int;
    pktSentACKTotal : int;
    pktRecvACKTotal : int;
    pktSentNAKTotal : int;
    pktRecvNAKTotal : int;
    usSndDurationTotal : int64;
    pktSndDropTotal : int;
    pktRcvDropTotal : int;
    pktRcvUndecryptTotal : int;
    byteSentTotal : UInt64.t;
    byteRecvTotal : UInt64.t;
    byteRetransTotal : UInt64.t;
    byteSndDropTotal : UInt64.t;
    byteRcvDropTotal : UInt64.t;
    byteRcvUndecryptTotal : UInt64.t;
    pktSent : int64;
    pktRecv : int64;
    pktSndLoss : int;
    pktRcvLoss : int;
    pktRetrans : int;
    pktRcvRetrans : int;
    pktSentACK : int;
    pktRecvACK : int;
    pktSentNAK : int;
    pktRecvNAK : int;
    mbpsSendRate : float;
    mbpsRecvRate : float;
    usSndDuration : int64;
    pktReorderDistance : int;
    pktRcvAvgBelatedTime : float;
    pktRcvBelated : int64;
    pktSndDrop : int;
    pktRcvDrop : int;
    pktRcvUndecrypt : int;
    byteSent : UInt64.t;
    byteRecv : UInt64.t;
    byteRetrans : UInt64.t;
    byteSndDrop : UInt64.t;
    byteRcvDrop : UInt64.t;
    byteRcvUndecrypt : UInt64.t;
    usPktSndPeriod : float;
    pktFlowWindow : int;
    pktCongestionWindow : int;
    pktFlightSize : int;
    msRTT : float;
    mbpsBandwidth : float;
    byteAvailSndBuf : int;
    byteAvailRcvBuf : int;
    mbpsMaxBW : float;
    byteMSS : int;
    pktSndBuf : int;
    byteSndBuf : int;
    msSndBuf : int;
    msSndTsbPdDelay : int;
    pktRcvBuf : int;
    byteRcvBuf : int;
    msRcvBuf : int;
    msRcvTsbPdDelay : int;
    pktSndFilterExtraTotal : int;
    pktRcvFilterExtraTotal : int;
    pktRcvFilterSupplyTotal : int;
    pktRcvFilterLossTotal : int;
    pktSndFilterExtra : int;
    pktRcvFilterExtra : int;
    pktRcvFilterSupply : int;
    pktRcvFilterLoss : int;
  }

  let from_struct stats =
    {
      msTimeStamp = !@(stats |-> CBytePerfMon.msTimeStamp);
      pktSentTotal = !@(stats |-> CBytePerfMon.pktSentTotal);
      pktRecvTotal = !@(stats |-> CBytePerfMon.pktRecvTotal);
      pktSndLossTotal = !@(stats |-> CBytePerfMon.pktSndLossTotal);
      pktRcvLossTotal = !@(stats |-> CBytePerfMon.pktRcvLossTotal);
      pktRetransTotal = !@(stats |-> CBytePerfMon.pktRetransTotal);
      pktSentACKTotal = !@(stats |-> CBytePerfMon.pktSentACKTotal);
      pktRecvACKTotal = !@(stats |-> CBytePerfMon.pktRecvACKTotal);
      pktSentNAKTotal = !@(stats |-> CBytePerfMon.pktSentNAKTotal);
      pktRecvNAKTotal = !@(stats |-> CBytePerfMon.pktRecvNAKTotal);
      usSndDurationTotal = !@(stats |-> CBytePerfMon.usSndDurationTotal);
      pktSndDropTotal = !@(stats |-> CBytePerfMon.pktSndDropTotal);
      pktRcvDropTotal = !@(stats |-> CBytePerfMon.pktRcvDropTotal);
      pktRcvUndecryptTotal = !@(stats |-> CBytePerfMon.pktRcvUndecryptTotal);
      byteSentTotal = !@(stats |-> CBytePerfMon.byteSentTotal);
      byteRecvTotal = !@(stats |-> CBytePerfMon.byteRecvTotal);
      byteRetransTotal = !@(stats |-> CBytePerfMon.byteRetransTotal);
      byteSndDropTotal = !@(stats |-> CBytePerfMon.byteSndDropTotal);
      byteRcvDropTotal = !@(stats |-> CBytePerfMon.byteRcvDropTotal);
      byteRcvUndecryptTotal = !@(stats |-> CBytePerfMon.byteRcvUndecryptTotal);
      pktSent = !@(stats |-> CBytePerfMon.pktSent);
      pktRecv = !@(stats |-> CBytePerfMon.pktRecv);
      pktSndLoss = !@(stats |-> CBytePerfMon.pktSndLoss);
      pktRcvLoss = !@(stats |-> CBytePerfMon.pktRcvLoss);
      pktRetrans = !@(stats |-> CBytePerfMon.pktRetrans);
      pktRcvRetrans = !@(stats |-> CBytePerfMon.pktRcvRetrans);
      pktSentACK = !@(stats |-> CBytePerfMon.pktSentACK);
      pktRecvACK = !@(stats |-> CBytePerfMon.pktRecvACK);
      pktSentNAK = !@(stats |-> CBytePerfMon.pktSentNAK);
      pktRecvNAK = !@(stats |-> CBytePerfMon.pktRecvNAK);
      mbpsSendRate = !@(stats |-> CBytePerfMon.mbpsSendRate);
      mbpsRecvRate = !@(stats |-> CBytePerfMon.mbpsRecvRate);
      usSndDuration = !@(stats |-> CBytePerfMon.usSndDuration);
      pktReorderDistance = !@(stats |-> CBytePerfMon.pktReorderDistance);
      pktRcvAvgBelatedTime = !@(stats |-> CBytePerfMon.pktRcvAvgBelatedTime);
      pktRcvBelated = !@(stats |-> CBytePerfMon.pktRcvBelated);
      pktSndDrop = !@(stats |-> CBytePerfMon.pktSndDrop);
      pktRcvDrop = !@(stats |-> CBytePerfMon.pktRcvDrop);
      pktRcvUndecrypt = !@(stats |-> CBytePerfMon.pktRcvUndecrypt);
      byteSent = !@(stats |-> CBytePerfMon.byteSent);
      byteRecv = !@(stats |-> CBytePerfMon.byteRecv);
      byteRetrans = !@(stats |-> CBytePerfMon.byteRetrans);
      byteSndDrop = !@(stats |-> CBytePerfMon.byteSndDrop);
      byteRcvDrop = !@(stats |-> CBytePerfMon.byteRcvDrop);
      byteRcvUndecrypt = !@(stats |-> CBytePerfMon.byteRcvUndecrypt);
      usPktSndPeriod = !@(stats |-> CBytePerfMon.usPktSndPeriod);
      pktFlowWindow = !@(stats |-> CBytePerfMon.pktFlowWindow);
      pktCongestionWindow = !@(stats |-> CBytePerfMon.pktCongestionWindow);
      pktFlightSize = !@(stats |-> CBytePerfMon.pktFlightSize);
      msRTT = !@(stats |-> CBytePerfMon.msRTT);
      mbpsBandwidth = !@(stats |-> CBytePerfMon.mbpsBandwidth);
      byteAvailSndBuf = !@(stats |-> CBytePerfMon.byteAvailSndBuf);
      byteAvailRcvBuf = !@(stats |-> CBytePerfMon.byteAvailRcvBuf);
      mbpsMaxBW = !@(stats |-> CBytePerfMon.mbpsMaxBW);
      byteMSS = !@(stats |-> CBytePerfMon.byteMSS);
      pktSndBuf = !@(stats |-> CBytePerfMon.pktSndBuf);
      byteSndBuf = !@(stats |-> CBytePerfMon.byteSndBuf);
      msSndBuf = !@(stats |-> CBytePerfMon.msSndBuf);
      msSndTsbPdDelay = !@(stats |-> CBytePerfMon.msSndTsbPdDelay);
      pktRcvBuf = !@(stats |-> CBytePerfMon.pktRcvBuf);
      byteRcvBuf = !@(stats |-> CBytePerfMon.byteRcvBuf);
      msRcvBuf = !@(stats |-> CBytePerfMon.msRcvBuf);
      msRcvTsbPdDelay = !@(stats |-> CBytePerfMon.msRcvTsbPdDelay);
      pktSndFilterExtraTotal = !@(stats |-> CBytePerfMon.pktSndFilterExtraTotal);
      pktRcvFilterExtraTotal = !@(stats |-> CBytePerfMon.pktRcvFilterExtraTotal);
      pktRcvFilterSupplyTotal =
        !@(stats |-> CBytePerfMon.pktRcvFilterSupplyTotal);
      pktRcvFilterLossTotal = !@(stats |-> CBytePerfMon.pktRcvFilterLossTotal);
      pktSndFilterExtra = !@(stats |-> CBytePerfMon.pktSndFilterExtra);
      pktRcvFilterExtra = !@(stats |-> CBytePerfMon.pktRcvFilterExtra);
      pktRcvFilterSupply = !@(stats |-> CBytePerfMon.pktRcvFilterSupply);
      pktRcvFilterLoss = !@(stats |-> CBytePerfMon.pktRcvFilterLoss);
    }

  let bstats ?(clear = false) socket =
    let clear = if clear then 0 else 1 in
    let stats = allocate_n CBytePerfMon.t ~count:1 in
    ignore (check_err (bstats socket stats clear));
    from_struct stats

  let bistats ?(clear = false) ?(instantaneous = false) socket =
    let clear = if clear then 0 else 1 in
    let instantaneous = if instantaneous then 0 else 1 in
    let stats = allocate_n CBytePerfMon.t ~count:1 in
    ignore (check_err (bistats socket stats clear instantaneous));
    from_struct stats
end

let cleanup () =
  cleanup ();
  Log.cleanup ()

let startup () =
  Log.startup ();
  startup ()
