open Avutil

external version : unit -> version = "ocaml_swresample_version"

let version = version ()

type u8ba =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type s16ba =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(* How a value of an OCaml type holds samples, spec/swresample.md §3.2. *)
type _ layout =
  | Interleaved_bytes : bytes layout
  | Planar_bytes : bytes array layout
  | Floats : float array layout
  | Planar_floats : float array array layout
  | Interleaved_bigarray : (_, _, Bigarray.c_layout) Bigarray.Array1.t layout
  | Planar_bigarray : (_, _, Bigarray.c_layout) Bigarray.Array1.t array layout
  | Frame : audio frame layout

(* [sample_format] is [None] when the format is given at creation. *)
type 'a kind = { layout : 'a layout; sample_format : Sample_format.t option }

module type AudioData = sig
  type t

  val kind : t kind
end

type dither_type = Swresample_options.dither_type
type engine = Swresample_options.engine
type filter_type = Swresample_options.filter_type
type options = [ dither_type | engine | filter_type ]
type ('i, 'o) ctx

(* The resampler option an element of [options] sets; the order is the one
   of the C side. *)
type setting =
  | Dither of dither_type
  | Engine of engine
  | Filter of filter_type

let setting : options -> setting = function
  | #dither_type as dither -> Dither dither
  | #engine as engine -> Engine engine
  | #filter_type as filter -> Filter filter

type side = {
  channel_layout : Channel_layout.t;
  sample_format : Sample_format.t;
  sample_rate : int;
}

external create_resampler : setting list -> side -> side -> ('i, 'o) ctx
  = "ocaml_swresample_create"

external is_planar : Sample_format.t -> bool = "ocaml_swresample_is_planar"

(* The C side converts between two shapes: frames, and planes held in
   bigarrays of any element type, one for interleaved samples and one per
   channel otherwise. The other kinds are brought to planes here. *)
type plane = Plane : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> plane
type samples = Sample_planes of plane array | Sample_frame of audio frame

(* [raw] asks for planes of bytes, whatever the sample format. *)
external convert_to_planes :
  raw:bool ->
  (_, _) ctx ->
  samples option ->
  int ->
  int ->
  (_, _, Bigarray.c_layout) Bigarray.Array1.t array
  = "ocaml_swresample_convert_to_planes"

external convert_to_frame :
  (_, _) ctx -> samples option -> int -> int -> audio frame
  = "ocaml_swresample_convert_to_frame"

external data_of_bytes : bytes -> data = "ocaml_swresample_data_of_bytes"
external bytes_of_data : data -> bytes = "ocaml_swresample_bytes_of_data"

let failure message = raise (Error (`Failure message))

(* Both copies turn a NaN into 0. The range is the caller's to check. *)
external plane_of_floats : float array -> int -> int -> f64ba
  = "ocaml_swresample_plane_of_floats"

external floats_of_plane : f64ba -> float array
  = "ocaml_swresample_floats_of_plane"

let plane_of_floats ?(offset = 0) ?length samples =
  let length = Option.value length ~default:(Array.length samples - offset) in
  Plane (plane_of_floats samples offset length)

let samples : type a. a kind -> a -> samples =
 fun kind value ->
  match kind.layout with
    | Frame -> Sample_frame value
    | Interleaved_bigarray -> Sample_planes [| Plane value |]
    | Planar_bigarray ->
        Sample_planes (Array.map (fun plane -> Plane plane) value)
    | Interleaved_bytes -> Sample_planes [| Plane (data_of_bytes value) |]
    | Planar_bytes ->
        Sample_planes
          (Array.map (fun plane -> Plane (data_of_bytes plane)) value)
    | Floats -> Sample_planes [| plane_of_floats value |]
    | Planar_floats -> Sample_planes (Array.map plane_of_floats value)

(* Planar float arrays with only the selected range copied; [None] leaves
   the range, and the failure of an invalid one, to the C side. *)
let selected_floats : type a. a kind -> a -> int -> int -> samples option =
 fun kind value offset length ->
  match kind.layout with
    | Planar_floats when Array.length value > 0 ->
        let available = Array.length value.(0) in
        let length = if length < 0 then available - offset else length in
        if
          length >= 0
          && offset + length <= available
          && Array.for_all (fun plane -> Array.length plane = available) value
        then
          Some
            (Sample_planes (Array.map (plane_of_floats ~offset ~length) value))
        else None
    | _ -> None

let converted : type a.
    a kind -> (_, _) ctx -> samples option -> int -> int -> a =
 fun kind resampler input offset length ->
  let planes ~raw = convert_to_planes ~raw resampler input offset length in
  match kind.layout with
    | Frame -> convert_to_frame resampler input offset length
    | Interleaved_bigarray -> (planes ~raw:false).(0)
    | Planar_bigarray -> planes ~raw:false
    | Interleaved_bytes -> bytes_of_data (planes ~raw:true).(0)
    | Planar_bytes -> Array.map bytes_of_data (planes ~raw:true)
    | Floats -> floats_of_plane (planes ~raw:false).(0)
    | Planar_floats -> Array.map floats_of_plane (planes ~raw:false)

(* The sample format of a side, spec/swresample.md §4.4. *)
let side_format : type a. a kind -> Sample_format.t option -> Sample_format.t =
 fun kind given ->
  let format =
    match (kind.sample_format, given) with
      | Some fixed, None -> fixed
      | Some fixed, Some given ->
          if given <> fixed then
            failure "the sample format is not the one of the data kind";
          fixed
      | None, Some given -> given
      | None, None -> failure "the data kind needs a sample format"
  in
  (match kind.layout with
    | Interleaved_bytes when is_planar format ->
        failure "interleaved bytes need a packed sample format"
    | _ -> ());
  format

module Make (I : AudioData) (O : AudioData) = struct
  type t = (I.t, O.t) ctx

  let create ?(options = []) in_layout ?in_sample_format in_rate out_layout
      ?out_sample_format out_rate : t =
    let in_format = side_format I.kind in_sample_format in
    let out_format = side_format O.kind out_sample_format in
    if in_rate < 1 || out_rate < 1 then failure "sample rate below 1";
    create_resampler (List.map setting options)
      {
        channel_layout = in_layout;
        sample_format = in_format;
        sample_rate = in_rate;
      }
      {
        channel_layout = out_layout;
        sample_format = out_format;
        sample_rate = out_rate;
      }

  let codec_side params =
    {
      channel_layout = Avcodec.Audio.get_channel_layout params;
      sample_format = Avcodec.Audio.get_sample_format params;
      sample_rate = Avcodec.Audio.get_sample_rate params;
    }

  let from_codec ?options params out_layout ?out_sample_format out_rate =
    let input = codec_side params in
    create ?options input.channel_layout ~in_sample_format:input.sample_format
      input.sample_rate out_layout ?out_sample_format out_rate

  let to_codec ?options in_layout ?in_sample_format in_rate params =
    let output = codec_side params in
    create ?options in_layout ?in_sample_format in_rate output.channel_layout
      ~out_sample_format:output.sample_format output.sample_rate

  let from_codec_to_codec ?options in_params out_params =
    let output = codec_side out_params in
    from_codec ?options in_params output.channel_layout
      ~out_sample_format:output.sample_format output.sample_rate

  (* A length of -1 is everything after the offset. *)
  let convert ?(offset = 0) ?length resampler input =
    if
      offset < 0
      || Option.fold ~none:false ~some:(fun length -> length < 0) length
    then failure "negative offset or length";
    let length = Option.value length ~default:(-1) in
    match selected_floats I.kind input offset length with
      | Some planes -> converted O.kind resampler (Some planes) 0 (-1)
      | None ->
          converted O.kind resampler (Some (samples I.kind input)) offset length

  let flush resampler = converted O.kind resampler None 0 0
end

module Bytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = None }
end

module FloatArray = struct
  type t = float array

  let kind = { layout = Floats; sample_format = Some `Dbl }
end

module PlanarFloatArray = struct
  type t = float array array

  let kind = { layout = Planar_floats; sample_format = Some `Dblp }
end

module Frame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = None }
end

module U8Bytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = Some `U8 }
end

module S16Bytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = Some `S16 }
end

module S32Bytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = Some `S32 }
end

module FltBytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = Some `Flt }
end

module DblBytes = struct
  type t = bytes

  let kind = { layout = Interleaved_bytes; sample_format = Some `Dbl }
end

module U8PlanarBytes = struct
  type t = bytes array

  let kind = { layout = Planar_bytes; sample_format = Some `U8p }
end

module S16PlanarBytes = struct
  type t = bytes array

  let kind = { layout = Planar_bytes; sample_format = Some `S16p }
end

module S32PlanarBytes = struct
  type t = bytes array

  let kind = { layout = Planar_bytes; sample_format = Some `S32p }
end

module FltPlanarBytes = struct
  type t = bytes array

  let kind = { layout = Planar_bytes; sample_format = Some `Fltp }
end

module DblPlanarBytes = struct
  type t = bytes array

  let kind = { layout = Planar_bytes; sample_format = Some `Dblp }
end

module U8BigArray = struct
  type t = u8ba

  let kind = { layout = Interleaved_bigarray; sample_format = Some `U8 }
end

module S16BigArray = struct
  type t = s16ba

  let kind = { layout = Interleaved_bigarray; sample_format = Some `S16 }
end

module S32BigArray = struct
  type t = s32ba

  let kind = { layout = Interleaved_bigarray; sample_format = Some `S32 }
end

module FltBigArray = struct
  type t = f32ba

  let kind = { layout = Interleaved_bigarray; sample_format = Some `Flt }
end

module DblBigArray = struct
  type t = f64ba

  let kind = { layout = Interleaved_bigarray; sample_format = Some `Dbl }
end

module U8PlanarBigArray = struct
  type t = u8ba array

  let kind = { layout = Planar_bigarray; sample_format = Some `U8p }
end

module S16PlanarBigArray = struct
  type t = s16ba array

  let kind = { layout = Planar_bigarray; sample_format = Some `S16p }
end

module S32PlanarBigArray = struct
  type t = s32ba array

  let kind = { layout = Planar_bigarray; sample_format = Some `S32p }
end

module FltPlanarBigArray = struct
  type t = f32ba array

  let kind = { layout = Planar_bigarray; sample_format = Some `Fltp }
end

module DblPlanarBigArray = struct
  type t = f64ba array

  let kind = { layout = Planar_bigarray; sample_format = Some `Dblp }
end

module U8Frame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `U8 }
end

module S16Frame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `S16 }
end

module S32Frame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `S32 }
end

module FltFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `Flt }
end

module DblFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `Dbl }
end

module U8PlanarFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `U8p }
end

module S16PlanarFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `S16p }
end

module S32PlanarFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `S32p }
end

module FltPlanarFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `Fltp }
end

module DblPlanarFrame = struct
  type t = audio frame

  let kind = { layout = Frame; sample_format = Some `Dblp }
end
