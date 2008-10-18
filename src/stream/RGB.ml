type t

type color = int * int * int * int

let rgb_of_int n =
  if n > 0xffffff then raise (Invalid_argument "Not a color");
  n land 0xff, (n lsr 8) land 0xff, (n lsr 16) land 0xff

external create : int -> int -> t = "caml_rgb_create"

external get_width : t -> int = "caml_rgb_get_width" "noalloc"

external get_height : t -> int = "caml_rgb_get_height" "noalloc"

external copy : t -> t = "caml_rgb_copy"

external blit : t -> t -> unit = "caml_rgb_blit" "noalloc"

external blit_off : t -> t -> int -> int -> unit = "caml_rgb_blit_off" "noalloc"

external fill : t -> color -> unit = "caml_rgb_fill" "noalloc"

external of_YUV420 : string * string * string -> t -> unit = "caml_rgb_of_YUV420" "noalloc"

let of_YUV420 (y, u, v) width =
  let height = String.length y / width in
  let ans = create width height in
    of_YUV420 (y, u, v) ans;
    ans

external to_YUV420 : t -> string * string * string = "caml_rgb_to_YUV420"

external get_pixel : t -> int -> int -> color = "caml_rgb_get_pixel"

external set_pixel : t -> int -> int -> color -> unit = "caml_rgb_set_pixel" "noalloc"

external randomize : t -> unit = "caml_rgb_randomize" "noalloc"

external scale : t -> t -> unit = "caml_rgb_scale" "noalloc"

let scale_to src w h =
  let dst = create w h in
    scale dst src;
    dst

external proportional_scale : t -> t -> unit = "caml_rgb_proportional_scale" "noalloc"

let proportional_scale_to src w h =
  let dst = create w h in
    proportional_scale dst src;
    dst

external to_bmp : t -> string = "caml_rgb_to_bmp"

let save_bmp f fname =
  let oc = open_out_bin fname in
    output_string oc (to_bmp f);
    close_out oc

exception Invalid_format of string

let ppm_header = Str.regexp "P6\n\\([0-9]+\\) \\([0-9]+\\)\n\\([0-9]+\\)\n"

let of_ppm ?alpha data =
  (
    try
      if not (Str.string_partial_match ppm_header data 0) then
        raise (Invalid_format "Not a PPM file.");
    with
      | _ -> raise (Invalid_format "Not a PPM file.")
  );
  let w = int_of_string (Str.matched_group 1 data) in
  let h = int_of_string (Str.matched_group 2 data) in
  let d = int_of_string (Str.matched_group 3 data) in
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

external greyscale : t -> unit = "caml_rgb_greyscale" "noalloc"

external invert : t -> unit = "caml_rgb_invert" "noalloc"

external add : t -> t -> unit = "caml_rgb_add" "noalloc"

external rotate : t -> float -> unit = "caml_rgb_rotate" "noalloc"

external scale_opacity : t -> float -> unit = "caml_rgb_scale_opacity" "noalloc"

external affine : t -> float -> float -> int -> int -> unit = "caml_rgb_affine" "noalloc"

external mask : t -> t -> unit = "caml_rgb_mask" "noalloc"

external lomo : t -> unit = "caml_rgb_lomo"
