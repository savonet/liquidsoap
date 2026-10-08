(** Bindings to libswscale. The interface is the one of spec/swscale.md §4. *)

open Avutil

(** The version of the libswscale loaded at run time, read once when the module
    is loaded. *)
val version : version

type pixel_format = Avutil.Pixel_format.t

(** A flag of a scaler; the flags of a list are combined. [Fast_bilinear],
    [Bilinear] and [Bicubic] select the scaling algorithm. [Print_info] makes
    libswscale log its scaling parameters, through FFmpeg's logging. *)
type flag = Fast_bilinear | Bilinear | Bicubic | Print_info

(** A scaler bound to an input size and pixel format and an output size and
    pixel format. It scales on one thread and works on caller-supplied
    bigarrays, in place, with {!scale}. It owns its native scaler, released when
    the value is collected. *)
type t

(** [create flags in_width in_height in_format out_width out_height out_format]
    creates a scaler. The arguments are, in order: the flags, the input width
    and height in pixels, the input pixel format, the output width and height in
    pixels and the output pixel format. Every other setting keeps FFmpeg's
    default.

    The call releases the OCaml runtime lock while the scaler is initialised.
    @raise Avutil.Error
      with [`Failure _] when a width or a height is below 1; with the FFmpeg
      error when FFmpeg rejects a setting or fails to initialise the scaler. *)
val create :
  flag list -> int -> int -> pixel_format -> int -> int -> pixel_format -> t

(** The buffers of an image: one [(plane, line_size)] pair per plane of the
    pixel format, in plane order. [line_size] is in bytes. A paletted format has
    its palette as one more buffer after the plane. *)
type planes = (data * int) array

(** [scale scaler src y h dst off] scales a slice of the input image: the [h]
    rows that start at row [y]. [src] holds the planes of that slice and [dst]
    the planes the output is written to.

    [off] is a number of rows: the output image starts at row [off] of the [dst]
    planes. For a plane with vertical chroma subsampling the row count is
    reduced accordingly.

    libswscale reads [src] and writes [dst] in place; nothing is copied. The
    call releases the OCaml runtime lock and excludes every other operation on
    the scaler: a concurrent one raises [Error (`Failure "Object in use!")] at
    once. After a failure the scaler stays usable.
    @raise Avutil.Error
      with [`Failure _], before anything is read or written, when [src] or [dst]
      holds fewer buffers than its pixel format needs, when [y], [h] or [off] is
      negative, when the slice lies outside the input height, when [off] is not
      a multiple of the vertical chroma subsampling factor, when a line size is
      smaller than the format needs for the image width and when a plane is
      shorter than the rows read from or written to it; with the FFmpeg error
      when scaling fails. *)
val scale : t -> planes -> int -> int -> planes -> int -> unit

(** How a value of type ['a] holds an image. Only the four data modules at the
    end of this interface provide values of this type. *)
type 'a kind

(** A data type a scaler reads or writes: one of {!BigArray}, {!PackedBigArray},
    {!Frame} and {!Bytes}. A program passes two of them to {!Make}. *)
module type VideoData = sig
  type t

  val kind : t kind
end

(** A scaler from images of type ['i] to images of type ['o]. It owns its native
    scaler, released when the value is collected, and keeps nothing of the
    values given to it or returned by it. *)
type ('i, 'o) ctx

(** Scalers that read images of type [I.t] and return images of type [O.t]. *)
module Make (I : VideoData) (O : VideoData) : sig
  type t = (I.t, O.t) ctx

  (** [create ?threads flags in_width in_height in_format out_width out_height
       out_format] creates a scaler, with the positional arguments of
      {!Swscale.create}: the flags, the input width, height and pixel format,
      then the output width, height and pixel format.
      @param threads
        The number of threads the scaling of one image is split across, 1 by
        default. With more than one, libswscale runs worker threads of its own
        during a conversion; the output is byte for byte the output of one
        thread.
      @raise Avutil.Error
        as {!Swscale.create}, and with [`Failure _] when the output pixel format
        is paletted and [O] is not {!Frame}: only a frame carries the palette
        back. *)
  val create :
    ?threads:int ->
    flag list ->
    int ->
    int ->
    pixel_format ->
    int ->
    int ->
    pixel_format ->
    t

  (** [convert scaler image] converts one whole image and returns the result as
      a new value.

      [image] is checked against the input side of the scaler before anything is
      read. A frame has the width, height and pixel format of the scaler. Planes
      hold at least the buffers the pixel format needs, the palette of a
      paletted format included; each has a line size wide enough for the width
      and a length that covers the height. Buffers after the ones the format
      needs are ignored.

      A result made of planes has one plane per plane of the output format, each
      with the smallest line size the format allows for the width and exactly
      the length the plane needs: a subsampled chroma plane is shorter. These
      sizes are the same on every call. A frame returned has the output width,
      height and pixel format, aligned buffers, and neither timestamp nor any
      other property.

      Bigarrays and frames given as input are read in place, as they are.
      Strings are copied into buffers with the padding libswscale may read past
      the end of a plane, and the result is copied into new strings. The
      bigarrays returned carry that padding beyond their length.

      The call releases the OCaml runtime lock and excludes every other
      operation on the scaler: a concurrent one raises
      [Error (`Failure "Object in use!")] at once. After a failure the scaler
      stays usable.
      @raise Avutil.Error
        with [`Failure _] when [image] fails a check above, and when the two
        arrays of a {!PackedBigArray} value differ in length; with the FFmpeg
        error when scaling fails. *)
  val convert : t -> I.t -> O.t
end

(** One [(plane, line_size)] pair per plane, the planes in bigarrays. *)
module BigArray : VideoData with type t = planes

(** The planes in one array and their line sizes, in bytes, in a second array of
    the same length. *)
module PackedBigArray : VideoData with type t = data array * int array

(** Video frames. *)
module Frame : VideoData with type t = video frame

(** One [(plane, line_size)] pair per plane, the planes in strings. Each
    conversion copies them. *)
module Bytes : VideoData with type t = (string * int) array
