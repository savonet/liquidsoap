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

type side = Channel_layout.t * Sample_format.t * int

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
  (_, _) ctx ->
  samples option ->
  int ->
  int ->
  raw:bool ->
  (_, _, Bigarray.c_layout) Bigarray.Array1.t array
  = "ocaml_swresample_convert_to_planes"

external convert_to_frame :
  (_, _) ctx -> samples option -> int -> int -> audio frame
  = "ocaml_swresample_convert_to_frame"

external data_of_bytes : bytes -> data = "ocaml_swresample_data_of_bytes"
external bytes_of_data : data -> bytes = "ocaml_swresample_bytes_of_data"

let failure message = raise (Error (`Failure message))

(* The loops below keep samples unboxed; a NaN becomes 0. *)
let plane_of_floats (samples : float array) =
  let length = Array.length samples in
  let plane =
    Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout length
  in
  for i = 0 to length - 1 do
    let sample = Array.unsafe_get samples i in
    Bigarray.Array1.unsafe_set plane i
      (if Float.is_nan sample then 0. else sample)
  done;
  Plane plane

let floats_of_plane (plane : f64ba) =
  let length = Bigarray.Array1.dim plane in
  let samples = Array.create_float length in
  for i = 0 to length - 1 do
    let sample = Bigarray.Array1.unsafe_get plane i in
    Array.unsafe_set samples i (if Float.is_nan sample then 0. else sample)
  done;
  samples

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

let converted : type a.
    a kind -> (_, _) ctx -> samples option -> int -> int -> a =
 fun kind resampler input offset length ->
  let planes ~raw = convert_to_planes resampler input offset length ~raw in
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
      (in_layout, in_format, in_rate)
      (out_layout, out_format, out_rate)

  let codec_side params =
    Avcodec.Audio.
      ( get_channel_layout params,
        get_sample_format params,
        get_sample_rate params )

  let from_codec ?options params out_layout ?out_sample_format out_rate =
    let in_layout, in_sample_format, in_rate = codec_side params in
    create ?options in_layout ~in_sample_format in_rate out_layout
      ?out_sample_format out_rate

  let to_codec ?options in_layout ?in_sample_format in_rate params =
    let out_layout, out_sample_format, out_rate = codec_side params in
    create ?options in_layout ?in_sample_format in_rate out_layout
      ~out_sample_format out_rate

  let from_codec_to_codec ?options in_params out_params =
    let out_layout, out_sample_format, out_rate = codec_side out_params in
    from_codec ?options in_params out_layout ~out_sample_format out_rate

  (* A length of -1 is everything after the offset. *)
  let convert ?(offset = 0) ?length resampler input =
    if
      offset < 0
      || Option.fold ~none:false ~some:(fun length -> length < 0) length
    then failure "negative offset or length";
    converted O.kind resampler
      (Some (samples I.kind input))
      offset
      (Option.value length ~default:(-1))

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
