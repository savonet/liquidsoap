(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(* Register codecs. *)
(* TODO: for now the size of the output frames have to match the size of the
   vid. *)
module FFmpeg = struct
  external init : unit -> unit = "caml_ffmpeg_init"

  exception End_of_stream

  let () = Callback.register_exception "ffmpeg_exn_end_of_stream" End_of_stream

  module Decoder = struct
    type t

    external openfile : string -> t = "caml_ffmpeg_dec_openfile"
    external dump_format : t -> string -> unit = "caml_ffmpeg_dec_dump_format"
    external width : t -> int = "caml_ffmpeg_dec_width"
    external height : t -> int = "caml_ffmpeg_dec_height"

    external read_frame : t -> Video.frame -> unit
      = "caml_ffmpeg_dec_read_frame"

    external close : t -> unit = "caml_ffmpeg_dec_close"

    external set_target_size : t -> int -> int -> unit
      = "caml_ffmpeg_dec_set_target_size"

    external frame_rate : t -> float = "caml_ffmpeg_dec_fps"
  end

  module Encoder = struct
    type t

    external openfile : string -> int * int -> int -> int -> int -> t
      = "caml_ffmpeg_enc_openfile"

    external dump_format : t -> string -> unit = "caml_ffmpeg_enc_dump_format"

    external write_frame : t -> Image.RGBA32.t -> unit
      = "caml_ffmpeg_enc_write_frame"

    external close : t -> unit = "caml_ffmpeg_enc_close"
  end

  (*
  module Scale = struct
    type t

    external create : int * int -> int * int -> t = "caml_sws_create"

    external scale_to : t -> Video.frame -> Video.frame -> unit = "caml_sws_scale_to"
  end
  *)
end

module D = FFmpeg.Decoder
module E = FFmpeg.Encoder

class reader_of_file fname =
  (* TODO: we should do this only once *)
  let () = FFmpeg.init () in
  let ff = D.openfile fname in
  let () = D.dump_format ff fname in
  let width = D.width ff in
  let height = D.height ff in
  object (self)
    method frame_rate = D.frame_rate ff
    method width = width
    method height = height

    (*
  method set_target_size (w:int) (h:int) : unit =
    (* Not working yet *)
    assert false
    (* FFmpeg.set_target_size ff w h *)
  *)
    method private read_frame =
      let img = Image.RGBA32.create width height in
      D.read_frame ff img;
      img

    method read buf ofs len =
      let n = ref 0 in
      try
        while !n < len do
          buf.(ofs + !n) <- self#read_frame;
          incr n
        done;
        !n
      with FFmpeg.End_of_stream -> !n

    method close = D.close ff
  end

class writer_to_file fname fr w h br =
  let fr = Video.FPS.to_frac fr in
  let enc = E.openfile fname fr w h br in
  let () = E.dump_format enc fname in
  object (self)
    method write buf ofs len =
      for i = ofs to ofs + len - 1 do
        E.write_frame enc buf.(i)
      done

    method close = E.close enc
  end
