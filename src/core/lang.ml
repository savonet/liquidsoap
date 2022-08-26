include Liquidsoap_lang.Lang
include Core_types
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

let to_proto_doc ~syntax ~static doc =
  let item = new Doc.item ~sort:false doc in
  item#add_subsection "syntax" (Lazy.from_val (Doc.trivial syntax));
  item#add_subsection "static"
    (Lazy.from_val (Doc.trivial (string_of_bool static)));
  item

let add_protocol ~syntax ~doc ~static name resolver =
  let doc () = to_proto_doc ~syntax ~static doc in
  let doc = Lazy.from_fun doc in
  let spec = { Request.static; resolve = resolver } in
  Request.protocols#register ~doc name spec

let frame_kind_t kind =
  Core_types.frame_kind_t (Frame.find_audio kind) (Frame.find_video kind)
    (Frame.find_midi kind)

let format_t t = Core_types.format_t t
let kind_t t = Core_types.kind_t t
let kind_none_t = Core_types.kind_t Frame.none

let empty =
  Frame.mk_fields ~audio:Frame.none ~video:Frame.none ~midi:Frame.none ()

let any = Frame.mk_fields ~audio:`Any ~video:`Any ~midi:`Any ()

let internal =
  Frame.mk_fields ~audio:`Internal ~video:`Internal ~midi:`Internal ()

let kind_type_of_kind_format fields =
  let audio = Core_types.kind_t (Frame.find_audio fields) in
  let video = Core_types.kind_t (Frame.find_video fields) in
  let midi = Core_types.kind_t (Frame.find_midi fields) in
  frame_kind_t (Frame.mk_fields ~audio ~video ~midi ())
