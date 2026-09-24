include Liquidsoap_lang.Lang
include Lang_source
include Lang_encoder.L
module Flags = Liquidsoap_lang_data.Flags
module Http = Liq_http

let source_t = source_t ?pos:None
let abstract_source_t = abstract_source_t ?pos:None
let () = Hooks_implementations.register ()

(** Helpers for defining protocols. *)

let add_protocol ~syntax ~doc ~static ~mode name resolver =
  Doc.Protocol.add ~name ~doc ~syntax;
  let spec = { Request.mode; static; resolve = resolver } in
  Plug.register Request.protocols ~doc name spec

let frame_t base_type fields = Frame_type.make base_type fields
let internal_tracks_t () = Frame_type.internal_tracks ()
let pcm_audio_t () = Frame_type.pcm_audio ()

let format_t t =
  Type.make
    (Type.Constr
       (* The type has to be invariant because we don't want the sup mechanism to be used here, see #2806. *)
       { Type.constructor = "format"; Type.params = [(`Invariant, t)] })

module HttpTransport = struct
  include Value.MkCustom (struct
    type content = Http.transport

    let name = "http_transport"

    let to_json ~pos _ =
      Runtime_error.raise ~pos
        ~message:"Http transport cannot be represented as json" "json"

    let to_string transport = Printf.sprintf "<%s_transport>" transport#name
    let compare = Stdlib.compare
  end)

  let meths =
    [
      ( "name",
        ([], string_t),
        "Transport name",
        fun transport -> string transport#name );
      ( "protocol",
        ([], string_t),
        "Transport protocol",
        fun transport -> string transport#protocol );
      ( "default_port",
        ([], int_t),
        "Transport default port",
        fun transport -> int transport#default_port );
    ]

  let base_t = t

  let t =
    method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_base_value = to_value

  let to_value transport =
    meth (to_value transport)
      (List.map (fun (lbl, _, _, m) -> (lbl, m transport)) meths)
end

let http_transport_t = HttpTransport.t
let http_transport_base_t = HttpTransport.base_t
let to_http_transport = HttpTransport.of_value
let http_transport = HttpTransport.to_value
let base_http_transport = HttpTransport.to_base_value ?pos:None

let reloadable_http_transport_t =
  method_t http_transport_t
    [
      ( "reload",
        ([], fun_t [] unit_t),
        "Read the certificate and key again and use them for new connections. \
         Open connections keep the previous ones. Raises and keeps the \
         previous ones if they cannot be loaded." );
    ]

let reloadable_http_transport ~reload transport =
  meth (http_transport transport)
    [
      ( "reload",
        val_fun [] (fun _ ->
            reload ();
            unit) );
    ]

let reload_on_arg =
  ( "reload_on",
    nullable_t (getter_t bool_t),
    Some null,
    Some
      "Checked on each new connection in server mode: when `true`, the \
       certificate and key are read again as with `reload()`, and the previous \
       ones are kept if that fails. Defaults to once a day." )

let current_day () =
  let time = Unix.localtime (Unix.time ()) in
  (time.Unix.tm_year, time.Unix.tm_yday)

let once_a_day () =
  let last_day = Atomic.make (current_day ()) in
  fun () ->
    let day = current_day () in
    day <> Atomic.exchange last_day day

let reload_log = Log.make ["http"; "reload"]

let reloadable_server_config ~reload_on build =
  let reload_on =
    match to_option reload_on with
      | None -> once_a_day ()
      | Some reload_on -> fun () -> to_bool (to_getter reload_on ())
  in
  let config = Atomic.make None in
  let reload () =
    match Atomic.get config with
      | None -> ()
      | Some _ -> Atomic.set config (Some (build ()))
  in
  let current () =
    (match Atomic.get config with
      | None -> ignore (Atomic.compare_and_set config None (Some (build ())))
      | Some _ when reload_on () -> (
          try reload ()
          with exn ->
            reload_log#severe
              "Could not reload certificate, keeping the previous one: %s"
              (Printexc.to_string exn))
      | Some _ -> ());
    Option.get (Atomic.get config)
  in
  (current, reload)
