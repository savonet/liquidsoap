include Liquidsoap_lang.Lang
include Lang_source
include Lang_encoder.L
module Doc = Liquidsoap_lang.Doc

let audio_pcm = { Frame.audio = Frame.audio_pcm; video = `Any; midi = `Any }

let audio_params p =
  {
    Frame.audio = `Format (Content.Audio.lift_params p);
    video = `Any;
    midi = `Any;
  }

let audio_n n = { Frame.audio = Frame.audio_n n; video = `Any; midi = `Any }
let audio_mono = audio_params { Content.Contents.channel_layout = lazy `Mono }

let audio_stereo =
  audio_params { Content.Contents.channel_layout = lazy `Stereo }

let video_yuva420p =
  { Frame.audio = `Any; video = Frame.video_yuva420p; midi = `Any }

let midi = { Frame.audio = `Any; video = `Any; midi = Frame.midi_native }

let midi_n n =
  {
    Frame.audio = `Any;
    video = `Any;
    midi = `Format (Content.Midi.lift_params { Content.Contents.channels = n });
  }

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
