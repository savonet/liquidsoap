(** Bindings to libswresample. The interface is the one of spec/swresample.md
    §4. *)

open Avutil

(** The version of the libswresample loaded at run time, read once when the
    module is loaded. *)
val version : version

(** How a value of type ['a] holds audio samples: its layout (interleaved or
    planar bytes, float arrays or bigarrays, or a frame) and either a sample
    format or the fact that the format is given when a converter is created.
    Only the data modules at the end of this interface provide values of this
    type. *)
type 'a kind

(** A data type a converter reads or writes. The modules of this type are the
    ones at the end of this interface, from {!Bytes} to {!DblPlanarFrame}; a
    program passes two of them to {!Make}. *)
module type AudioData = sig
  type t

  val kind : t kind
end

(** The dithering method of the resampler. *)
type dither_type = Swresample_options.dither_type

(** The resampling engine. *)
type engine = Swresample_options.engine

(** The type of the resampling filter. *)
type filter_type = Swresample_options.filter_type

(** A setting of the resampler, given in the [?options] list of the creation
    functions. *)
type options = [ dither_type | engine | filter_type ]

(** A converter from data of type ['i] to data of type ['o]: it resamples,
    remixes channels and converts the sample format. It owns its native
    resampler, released when the value is collected, and keeps nothing of the
    values given to it or returned by it. Between calls it holds the samples the
    resampler buffers. *)
type ('i, 'o) ctx

(** Converters that read values of type [I.t] and return values of type [O.t].
*)
module Make (I : AudioData) (O : AudioData) : sig
  type t = (I.t, O.t) ctx

  (** [create ?options in_layout ?in_sample_format in_rate out_layout
       ?out_sample_format out_rate] creates a converter. The positional
      arguments are, in order: the input channel layout, the input sample rate
      in Hz, the output channel layout and the output sample rate in Hz.

      The sample format of each side comes from its data module. When the module
      fixes a format, as {!S16Bytes} or {!FloatArray} do, the optional argument
      is omitted, or equal to that format. When the module leaves the format
      open, as {!Bytes} and {!Frame} do, the optional argument is required. With
      {!Bytes} the format is a packed (interleaved) one.

      Both layouts are copied. Every other setting of the resampler keeps
      FFmpeg's default. The call releases the OCaml runtime lock while the
      resampler is initialised.
      @param options
        Settings applied in list order, none by default. A later element of the
        same type overrides an earlier one.
      @param in_sample_format The sample format of the input values.
      @param out_sample_format The sample format of the values returned.
      @raise Avutil.Error
        with [`Failure _] when a sample format is missing, differs from the one
        the data module fixes or is planar for {!Bytes}, and when a sample rate
        is below 1; with the FFmpeg error when FFmpeg rejects a setting or fails
        to initialise the resampler. *)
  val create :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  (** [from_codec ?options params out_layout ?out_sample_format out_rate] is
      {!create} with the input channel layout, sample format and sample rate
      read from the codec parameters [params]. The format read is given as
      [in_sample_format]: creation fails when [I] fixes another format. *)
  val from_codec :
    ?options:options list ->
    audio Avcodec.params ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  (** [to_codec ?options in_layout ?in_sample_format in_rate params] is
      {!create} with the output channel layout, sample format and sample rate
      read from the codec parameters [params]. The format read is given as
      [out_sample_format]: creation fails when [O] fixes another format. *)
  val to_codec :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    audio Avcodec.params ->
    t

  (** [from_codec_to_codec ?options in_params out_params] is {!create} with the
      channel layout, sample format and sample rate of the input read from
      [in_params] and those of the output read from [out_params]. Creation fails
      when a data module fixes a format other than the one read. *)
  val from_codec_to_codec :
    ?options:options list -> audio Avcodec.params -> audio Avcodec.params -> t

  (** [convert ?offset ?length converter input] converts samples of [input] and
      returns the samples the resampler produces, as a new value.

      The result holds exactly the samples produced. Their number is known only
      after the conversion: the resampler keeps samples buffered between calls,
      and {!flush} returns the ones it still holds. It can be 0, and the result
      is then an empty value of the same shape: empty planes, or a frame of zero
      samples. A frame returned has the output channel layout, sample format and
      sample rate, and no timestamp.

      [input] is checked against the input side of the converter. A planar value
      has one plane per channel, all of the same length. A frame has the channel
      count and the sample format of the converter; its sample rate and the
      arrangement of its channel layout are the caller's to match. A trailing
      partial sample group of an interleaved value is ignored.

      Bytes and float arrays are copied in and out. A NaN read from a float
      array, and a NaN written to one, becomes 0. Bigarrays and frames are read
      in place while the OCaml runtime lock is released, without any NaN
      replacement: what another thread writes to them during the call is visible
      to the conversion.

      The call excludes every other operation on the converter: a concurrent one
      raises [Error (`Failure "Object in use!")] at once. After a failure the
      converter stays usable.
      @param offset
        The first sample converted, counted in samples per channel. 0 by
        default.
      @param length
        The number of samples per channel converted. By default, everything
        after [offset].
      @raise Avutil.Error
        with [`Failure _] when [offset] or [length] is negative, when
        [offset + length] exceeds the samples per channel [input] holds and when
        [input] has not the shape described above; with the FFmpeg error when
        the conversion fails. *)
  val convert : ?offset:int -> ?length:int -> t -> I.t -> O.t

  (** [flush converter] returns the samples the resampler still holds: the tail
      a rate conversion keeps back until it knows that no input follows. After
      it, the samples returned by all the calls since creation are everything
      the input produces.

      A second [flush] with no conversion in between returns an empty value. The
      converter stays usable: {!convert} may be called again. The call releases
      the OCaml runtime lock and excludes other operations on the converter, as
      {!convert} does.
      @raise Avutil.Error when the conversion fails. *)
  val flush : t -> O.t
end

(** Unsigned 8-bit samples. *)
type u8ba =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Signed 16-bit samples. *)
type s16ba =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Signed 32-bit samples. *)
type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** 32-bit float samples. *)
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** 64-bit float samples. *)
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Raw samples in [bytes], channels interleaved in the order of the channel
    layout, each sample in the machine's native representation of the sample
    format. The format is given at creation and is a packed one. With stereo
    signed 16-bit samples, three samples per channel take 12 bytes:
    [L0 R0 L1 R1 L2 R2]. *)
module Bytes : AudioData with type t = bytes

(** Samples as OCaml floats, channels interleaved: [l0 r0 l1 r1 ...]. The sample
    format is [`Dbl]. *)
module FloatArray : AudioData with type t = float array

(** One [float array] of samples per channel, in the order of the channel
    layout. The sample format is [`Dblp]. *)
module PlanarFloatArray : AudioData with type t = float array array

(** Audio frames of the sample format given at creation, packed or planar. *)
module Frame : AudioData with type t = audio frame

(** As {!Bytes}, with the sample format [`U8]. *)
module U8Bytes : AudioData with type t = bytes

(** As {!Bytes}, with the sample format [`S16]. *)
module S16Bytes : AudioData with type t = bytes

(** As {!Bytes}, with the sample format [`S32]. *)
module S32Bytes : AudioData with type t = bytes

(** As {!Bytes}, with the sample format [`Flt]. *)
module FltBytes : AudioData with type t = bytes

(** As {!Bytes}, with the sample format [`Dbl]. *)
module DblBytes : AudioData with type t = bytes

(** One [bytes] of raw samples per channel, in the order of the channel layout,
    each sample in the machine's native representation. The sample format is
    [`U8p]. *)
module U8PlanarBytes : AudioData with type t = bytes array

(** As {!U8PlanarBytes}, with the sample format [`S16p]. *)
module S16PlanarBytes : AudioData with type t = bytes array

(** As {!U8PlanarBytes}, with the sample format [`S32p]. *)
module S32PlanarBytes : AudioData with type t = bytes array

(** As {!U8PlanarBytes}, with the sample format [`Fltp]. *)
module FltPlanarBytes : AudioData with type t = bytes array

(** As {!U8PlanarBytes}, with the sample format [`Dblp]. *)
module DblPlanarBytes : AudioData with type t = bytes array

(** One bigarray, one element per sample, channels interleaved. The sample
    format is [`U8]. An input bigarray is read in place; a bigarray returned is
    a new one. *)
module U8BigArray : AudioData with type t = u8ba

(** As {!U8BigArray}, with the sample format [`S16]. *)
module S16BigArray : AudioData with type t = s16ba

(** As {!U8BigArray}, with the sample format [`S32]. *)
module S32BigArray : AudioData with type t = s32ba

(** As {!U8BigArray}, with the sample format [`Flt]. *)
module FltBigArray : AudioData with type t = f32ba

(** As {!U8BigArray}, with the sample format [`Dbl]. *)
module DblBigArray : AudioData with type t = f64ba

(** One bigarray per channel, in the order of the channel layout, one element
    per sample. The sample format is [`U8p]. Input bigarrays are read in place;
    the bigarrays returned are new ones. *)
module U8PlanarBigArray : AudioData with type t = u8ba array

(** As {!U8PlanarBigArray}, with the sample format [`S16p]. *)
module S16PlanarBigArray : AudioData with type t = s16ba array

(** As {!U8PlanarBigArray}, with the sample format [`S32p]. *)
module S32PlanarBigArray : AudioData with type t = s32ba array

(** As {!U8PlanarBigArray}, with the sample format [`Fltp]. *)
module FltPlanarBigArray : AudioData with type t = f32ba array

(** As {!U8PlanarBigArray}, with the sample format [`Dblp]. *)
module DblPlanarBigArray : AudioData with type t = f64ba array

(** Audio frames of the sample format [`U8]. *)
module U8Frame : AudioData with type t = audio frame

(** Audio frames of the sample format [`S16]. *)
module S16Frame : AudioData with type t = audio frame

(** Audio frames of the sample format [`S32]. *)
module S32Frame : AudioData with type t = audio frame

(** Audio frames of the sample format [`Flt]. *)
module FltFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`Dbl]. *)
module DblFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`U8p]. *)
module U8PlanarFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`S16p]. *)
module S16PlanarFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`S32p]. *)
module S32PlanarFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`Fltp]. *)
module FltPlanarFrame : AudioData with type t = audio frame

(** Audio frames of the sample format [`Dblp]. *)
module DblPlanarFrame : AudioData with type t = audio frame
