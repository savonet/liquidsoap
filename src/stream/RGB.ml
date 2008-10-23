type t

type color = int * int * int * int

let rgb_of_int n =
  if n > 0xffffff then raise (Invalid_argument "Not a color");
  (n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff

external create : int -> int -> t = "caml_rgb_create"

external to_ba : t -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t  = "caml_rgb_to_ba"

external of_ba : int -> int -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> t = "caml_rgb_of_ba"

external unlock_frame : t -> unit = "caml_rgb_unlock_value" "noalloc"

external unlock_ba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit = "caml_rgb_unlock_value"

external get_width : t -> int = "caml_rgb_get_width" "noalloc"

external get_height : t -> int = "caml_rgb_get_height" "noalloc"

let get_dims f = get_width f, get_height f

external copy : t -> t = "caml_rgb_copy"

external blit : t -> t -> unit = "caml_rgb_blit" "noalloc"

external blit_off : t -> t -> int -> int -> bool -> unit = "caml_rgb_blit_off" "noalloc"

external blit_off_scale : t -> t -> int * int -> int * int -> bool -> unit = "caml_rgb_blit_off_scale" "noalloc"

let blit_fast src dst =
  blit src dst

let blit ?(blank=true) ?(x=0) ?(y=0) ?w ?h src dst =
  match (w,h) with
    | None, None -> blit_off src dst x y blank
    | Some w, Some h -> blit_off_scale src dst (x,y) (w,h) blank
    | _, _ -> assert false

external fill : t -> color -> unit = "caml_rgb_fill" "noalloc"

external blank : t -> unit = "caml_rgb_blank" "noalloc"

external of_linear_rgb : t -> string -> unit = "caml_rgb_of_linear_rgb" "noalloc"

let of_linear_rgb data width =
  let height = (String.length data / 3) / width in
  let ans = create width height in
    of_linear_rgb ans data;
    ans

type yuv_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type yuv = (yuv_data *int ) * (yuv_data * yuv_data * int)

external of_YUV420 : yuv -> t -> unit = "caml_rgb_of_YUV420" "noalloc"

let of_YUV420_create frame width height =
  let ans = create width height in
    of_YUV420 frame ans;
    ans

external create_yuv : int -> int -> yuv = "caml_yuv_create"

external to_YUV420 : t -> yuv -> unit = "caml_rgb_to_YUV420" "noalloc"

external get_pixel : t -> int -> int -> color = "caml_rgb_get_pixel"

external set_pixel : t -> int -> int -> color -> unit = "caml_rgb_set_pixel" "noalloc"

external randomize : t -> unit = "caml_rgb_randomize" "noalloc"

external scale_coef : t -> t -> int * int -> int * int -> unit = "caml_rgb_scale" "noalloc"

external bilinear_scale_coef : t -> t -> float -> float -> unit = "caml_rgb_bilinear_scale" "noalloc"

let scale src dst =
  let sw, sh = get_dims src in
  let dw, dh = get_dims dst in
    scale_coef dst src (dw, sw) (dh, sh)

let scale_to src w h =
  let sw, sh = get_dims src in
  let dst = create w h in
    scale_coef dst src (w, sw) (h, sh);
    dst

let proportional_scale ?(bilinear=false) dst src =
  let sw, sh = get_dims src in
  let dw, dh = get_dims dst in
  let n, d =
    if dh * sw < sh * dw then
      dh, sh
    else
      dw, sw
  in
    if bilinear then
      let a = float_of_int n /. float_of_int d in
        bilinear_scale_coef dst src a a
    else
      scale_coef dst src (n, d) (n, d)

let proportional_scale_to ?(bilinear=false) src w h =
  let dst = create w h in
    proportional_scale ~bilinear dst src;
    dst

external to_bmp : t -> string = "caml_rgb_to_bmp"

let save_bmp f fname =
  let oc = open_out_bin fname in
    output_string oc (to_bmp f);
    close_out oc

exception Invalid_format of string

let ppm_header = Str.regexp "P6\n\\(#.*\n\\)?\\([0-9]+\\) \\([0-9]+\\)\n\\([0-9]+\\)\n"

let of_ppm ?alpha data =
  (
    try
      if not (Str.string_partial_match ppm_header data 0) then
        raise (Invalid_format "Not a PPM file.");
    with
      | _ -> raise (Invalid_format "Not a PPM file.")
  );
  let w = int_of_string (Str.matched_group 2 data) in
  let h = int_of_string (Str.matched_group 3 data) in
  let d = int_of_string (Str.matched_group 4 data) in
  let o = Str.match_end () in
  let datalen = String.length data - o in
    if d <> 255 then
      raise (Invalid_format (Printf.sprintf "Files of color depth %d are not handled." d));
    if datalen < 3*w*h then
      raise (Invalid_format (Printf.sprintf "Got %d bytes of data instead of expected %d." datalen (3*w*h)));
    let ans = create w h in
      for j = 0 to h - 1 do
        for i = 0 to w - 1 do
          let r, g, b =
            int_of_char data.[o + 3 * (j * w + i) + 0],
            int_of_char data.[o + 3 * (j * w + i) + 1],
            int_of_char data.[o + 3 * (j * w + i) + 2]
          in
          let a =
            match alpha with
              | Some (ra, ga, ba) -> if r = ra && g = ga && b = ba then 0x00 else 0xff
              | None -> 0xff
          in
            set_pixel ans i j (r, g, b, a);
        done
      done;
      ans

let read_ppm ?alpha fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let data = String.create len in
    really_input ic data 0 len;
    close_in ic;
    of_ppm ?alpha data

external to_int_image : t -> int array array = "caml_rgb_to_color_array"

external greyscale : t -> bool -> unit = "caml_rgb_greyscale" "noalloc"

let sepia buf = greyscale buf true

let greyscale buf = greyscale buf false

external invert : t -> unit = "caml_rgb_invert" "noalloc"

external add : t -> t -> unit = "caml_rgb_add" "noalloc"

external rotate : t -> float -> unit = "caml_rgb_rotate" "noalloc"

external scale_opacity : t -> float -> unit = "caml_rgb_scale_opacity" "noalloc"

external disk_opacity : t -> int -> int -> int -> unit = "caml_rgb_disk_opacity" "noalloc"

external affine : t -> float -> float -> int -> int -> unit = "caml_rgb_affine" "noalloc"

(* TODO: faster implementation? *)
let translate f x y =
  affine f 1. 1. x y

external mask : t -> t -> unit = "caml_rgb_mask" "noalloc"

external lomo : t -> unit = "caml_rgb_lomo" "noalloc"

external color_to_alpha : t -> int * int * int -> int -> unit = "caml_rgb_color_to_alpha" "noalloc"
