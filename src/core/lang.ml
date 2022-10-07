include Liquidsoap_lang.Lang
include Lang_source
include Lang_encoder.L
module Doc = Liquidsoap_lang.Doc

let audio_pcm = Frame.mk_fields ~audio:Frame.audio_pcm ~video:`Any ~midi:`Any ()

let audio_params p =
  Frame.mk_fields
    ~audio:(`Format (Content.Audio.lift_params p))
    ~video:`Any ~midi:`Any ()

let audio_n n =
  Frame.mk_fields ~audio:(Frame.audio_n n) ~video:`Any ~midi:`Any ()

let audio_mono = audio_params { Content.channel_layout = lazy `Mono }
let audio_stereo = audio_params { Content.channel_layout = lazy `Stereo }

let video_yuva420p =
  Frame.mk_fields ~audio:`Any ~video:Frame.video_yuva420p ~midi:`Any ()

let midi = Frame.mk_fields ~audio:`Any ~video:`Any ~midi:Frame.midi_native ()

let midi_n n =
  Frame.mk_fields ~audio:`Any ~video:`Any
    ~midi:(`Format (Content.Midi.lift_params { Content.channels = n }))
    ()

(** Helpers for defining protocols. *)

let add_protocol ~syntax ~doc ~static name resolver =
  Doc.Protocol.add ~name ~doc ~syntax ~static;
  let spec = { Request.static; resolve = resolver } in
  Plug.register Request.protocols ~doc name spec

let format_t t = format_t t
let kind_t k = Frame_type.make_kind k
let kind_none_t = kind_t Frame.none

let frame_t kind =
  Frame_type.make ~audio:(Frame.find_audio kind) ~video:(Frame.find_video kind)
    ~midi:(Frame.find_midi kind) ()

let of_frame_t t =
  let t = Type.deref t in
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params = [(_, audio); (_, video); (_, midi)];
        } ->
        Frame.mk_fields ~audio ~video ~midi ()
    | Type.Var ({ contents = Type.Free _ } as var) ->
        let audio = kind_t `Any in
        let video = kind_t `Any in
        let midi = kind_t `Any in
        var := Type.Link (`Invariant, Frame_type.make ~audio ~video ~midi ());
        Frame.mk_fields ~audio ~video ~midi ()
    | _ -> assert false

let of_source_t t =
  match (Type.deref t).Type.descr with
    | Type.Constr { Type.constructor = "source"; params = [(_, t)] } -> t
    | _ -> assert false

let empty =
  Frame.mk_fields ~audio:Frame.none ~video:Frame.none ~midi:Frame.none ()

let any = Frame.mk_fields ~audio:`Any ~video:`Any ~midi:`Any ()

let internal =
  Frame.mk_fields ~audio:`Internal ~video:`Internal ~midi:`Internal ()

let frame_kind_t fields =
  let audio = kind_t (Frame.find_audio fields) in
  let video = kind_t (Frame.find_video fields) in
  let midi = kind_t (Frame.find_midi fields) in
  frame_t (Frame.mk_fields ~audio ~video ~midi ())

module HttpTransport = struct
  include Value.MkAbstract (struct
    type content = Http.transport

    let name = "http_transport"

    let to_json _ =
      raise
        Runtime_error.(
          Runtime_error
            {
              kind = "json";
              msg = "Http transport cannot be represented as json";
              pos = [];
            })

    let descr transport = Printf.sprintf "<%s_transport>" transport#name
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

  let t =
    method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value transport =
    meth (to_value transport)
      (List.map (fun (lbl, _, _, m) -> (lbl, m transport)) meths)

  let of_value transport = of_value (demeth transport)
end

let http_transport_t = HttpTransport.t
let to_http_transport = HttpTransport.of_value
let http_transport = HttpTransport.to_value
