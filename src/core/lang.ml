include Liquidsoap_lang.Lang
include Lang_source
include Lang_encoder.L
module Flags = Liquidsoap_lang.Flags
module Http = Liq_http

let source_t = source_t ?pos:None
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
       { constructor = "format"; params = [(`Invariant, t)] })

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
