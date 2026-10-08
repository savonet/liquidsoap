open Avutil

external version : unit -> version = "ocaml_swscale_version"

let version = version ()

type pixel_format = Avutil.Pixel_format.t
type flag = Fast_bilinear | Bilinear | Bicubic | Print_info
type t
type geometry = { width : int; height : int; pixel_format : pixel_format }

external create_scaler : int -> flag list -> geometry -> geometry -> t
  = "ocaml_swscale_create"

let create flags width height pixel_format out_width out_height out_pixel_format
    =
  create_scaler 1 flags
    { width; height; pixel_format }
    { width = out_width; height = out_height; pixel_format = out_pixel_format }

type planes = (data * int) array

external scale : t -> planes -> int -> int -> planes -> int -> unit
  = "ocaml_swscale_scale_bytecode" "ocaml_swscale_scale"

type _ kind =
  | Planes : planes kind
  | Packed : (data array * int array) kind
  | Frame : video frame kind
  | Strings : (string * int) array kind

module type VideoData = sig
  type t

  val kind : t kind
end

type ('i, 'o) ctx = t

(* The C side converts between two shapes: planes held in bigarrays, and
   frames. The other kinds are brought to planes here. *)
type image = Image_planes of planes | Image_frame of video frame

external convert_to_planes : t -> image -> planes
  = "ocaml_swscale_convert_to_planes"

external convert_to_frame : t -> image -> video frame
  = "ocaml_swscale_convert_to_frame"

(* A bigarray plane holding a copy of a string, with the padding libswscale
   may read past a plane. *)
external plane_of_string : string -> data = "ocaml_swscale_plane_of_string"
external string_of_plane : data -> string = "ocaml_swscale_string_of_plane"

let failure message = raise (Error (`Failure message))

let image : type a. a kind -> a -> image =
 fun kind value ->
  match kind with
    | Planes -> Image_planes value
    | Frame -> Image_frame value
    | Packed ->
        let data, linesizes = value in
        if Array.length data <> Array.length linesizes then
          failure "the planes and the line sizes differ in number";
        Image_planes
          (Array.map2 (fun plane linesize -> (plane, linesize)) data linesizes)
    | Strings ->
        Image_planes
          (Array.map
             (fun (plane, linesize) -> (plane_of_string plane, linesize))
             value)

let converted : type a. a kind -> t -> image -> a =
 fun kind scaler image ->
  match kind with
    | Planes -> convert_to_planes scaler image
    | Frame -> convert_to_frame scaler image
    | Packed ->
        let planes = convert_to_planes scaler image in
        (Array.map fst planes, Array.map snd planes)
    | Strings ->
        Array.map
          (fun (plane, linesize) -> (string_of_plane plane, linesize))
          (convert_to_planes scaler image)

let is_paletted pixel_format =
  match Pixel_format.descriptor pixel_format with
    | descriptor -> List.mem `Pal descriptor.flags
    | exception Not_found -> false

module Make (I : VideoData) (O : VideoData) = struct
  type nonrec t = t

  let returns_frame =
    match O.kind with Frame -> true | Planes | Packed | Strings -> false

  let create ?(threads = 1) flags width height pixel_format out_width out_height
      out_pixel_format =
    if is_paletted out_pixel_format && not returns_frame then
      failure "a paletted output needs the frame kind";
    create_scaler threads flags
      { width; height; pixel_format }
      {
        width = out_width;
        height = out_height;
        pixel_format = out_pixel_format;
      }

  let convert scaler input = converted O.kind scaler (image I.kind input)
end

module BigArray = struct
  type t = planes

  let kind = Planes
end

module PackedBigArray = struct
  type t = data array * int array

  let kind = Packed
end

module Frame = struct
  type t = video frame

  let kind = Frame
end

module Bytes = struct
  type t = (string * int) array

  let kind = Strings
end
