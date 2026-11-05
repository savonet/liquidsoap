type fields = {
  bandwidth : int option;
  codecs : string option;
  extname : string option;
  video_size : (int * int) option;
  id3 : bool option;
  id3_version : int option;
  replay_id3 : bool;
  extra_tags : string list;
}

type metadata =
  [ `None
  | `Pending of Frame.Metadata.Export.t
  | `Sent of Frame.Metadata.Export.t ]

type segment = {
  start : int;
  mutable discontinuous : bool;
  mutable tags : string list;
  mutable init : string option;
}

(* The encoder's answers, read when they become valid: at its init segment
   and at each restart (spec/encoder-contract.md §6). *)
type properties = {
  codecs : string option;
  independent : bool;
  free_split : bool;
  bitrate : int option;
  video_size : (int * int) option;
}

type run = {
  encoder : Encoder.hls_encoder;
  segmenter : segment Segmenter.t;
  mutable properties : properties option;
  mutable last_tag : int option;
  mutable bandwidth_warned : bool;
}

type t = {
  name : string;
  format_value : Lang.value;
  format : Encoder.format;
  fields : fields;
  extname : string;
  id3_mode : Encoder.hls_id3_mode;
  factory : Encoder.hls_factory;
  mutable window : Window.t;
  tags_lock : Mutex.t;
  mutable pending_tags : string list;
  mutable pending_discontinuity : bool;
  mutable current_init : string option;
  mutable inits : string list;
  mutable metadata : metadata;
  mutable run : run option;
}

let take_tags v =
  Mutex.protect v.tags_lock (fun () ->
      let tags = v.pending_tags in
      v.pending_tags <- [];
      tags)

let give_back_tags v tags =
  Mutex.protect v.tags_lock (fun () -> v.pending_tags <- tags @ v.pending_tags)

let add_tag v tag =
  Mutex.protect v.tags_lock (fun () -> v.pending_tags <- v.pending_tags @ [tag])

let pending_tags v = Mutex.protect v.tags_lock (fun () -> v.pending_tags)

let read_properties (e : Encoder.hls_encoder) =
  {
    codecs = e.codec_attrs ();
    independent = e.independent_segments ();
    free_split = e.free_split ();
    bitrate = e.hls_bitrate ();
    video_size = e.hls_video_size ();
  }

let initialized v =
  match v.run with Some { properties = Some _; _ } -> true | _ -> false

let encoder_answer v f =
  match v.run with Some { properties = Some p; _ } -> f p | _ -> None

let headroom b = int_of_float (float_of_int b *. 1.1)
let nominal format = try Some (Encoder.bitrate format) with Not_found -> None
let measured v ~target = Measure.peak_bitrate ~target (Window.listed v.window)

(* spec/variants.md §3: the first source that answers. *)
let bandwidth v ~target =
  match v.fields.bandwidth with
    | Some b -> b
    | None -> (
        match encoder_answer v (fun p -> p.bitrate) with
          | Some b -> headroom b
          | None -> (
              match nominal v.format with
                | Some b -> headroom b
                | None ->
                    Option.fold ~none:0 ~some:headroom (measured v ~target)))

let codecs v =
  match v.fields.codecs with
    | Some c -> Some c
    | None -> (
        let listed =
          if initialized v then Measure.codecs (Window.listed v.window)
          else None
        in
        match listed with
          | Some c -> Some c
          | None -> (
              try Some (Encoder.iso_base_file_media_file_format v.format)
              with Not_found -> None))

let video_size v =
  match v.fields.video_size with
    | Some s -> Some s
    | None -> (
        match encoder_answer v (fun p -> p.video_size) with
          | Some s -> Some s
          | None -> Encoder.video_size v.format)

let segment_extension v = if v.extname = "mp4" then "m4s" else v.extname
let init_extension v = v.extname
