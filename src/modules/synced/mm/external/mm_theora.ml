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

open Mm_image
open Mm_video

class reader_of_file fname =
  let sync, fd = Ogg.Sync.create_from_file fname in
  let rec fill os =
    let page = Ogg.Sync.read sync in
    try
      (* We drop pages which are not for us.. *)
      if Ogg.Page.serialno page = Ogg.Stream.serialno os then
        Ogg.Stream.put_page os page
    with Ogg.Bad_data -> fill os
    (* Do not care about page that are not for us.. *)
  in
  (* Test whether the stream is theora *)
  let test_theora () =
    (* Get First page *)
    let page = Ogg.Sync.read sync in
    (* Check whether this is a b_o_s *)
    if not (Ogg.Page.bos page) then raise Video.IO.Invalid_file;
    (* Create a stream with this ID *)
    let serial = Ogg.Page.serialno page in
    Printf.printf "Testing stream %nx.\n" serial;
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page;
    let packet = Ogg.Stream.get_packet os in
    (* Test header. Do not catch anything, first page should be sufficient. *)
    if not (Theora.Decoder.check packet) then raise Not_found;
    Printf.printf "Got a theora stream!\n";
    let dec = Theora.Decoder.create () in
    (* Decode headers *)
    let rec f packet =
      try Theora.Decoder.headerin dec packet
      with Ogg.Not_enough_data ->
        let rec g () =
          try
            let packet = Ogg.Stream.get_packet os in
            f packet
          with Ogg.Not_enough_data ->
            fill os;
            g ()
        in
        g ()
    in
    let dec, info, _, _ = f packet in
    (os, dec, info)
  in
  (* Now find a theora stream *)
  let rec init () =
    try test_theora () with
      | Not_found ->
          Printf.printf "This stream was not theora..\n";
          init ()
      | Video.IO.Invalid_file as e ->
          Printf.printf "No theora stream was found..\n%!";
          raise e
  in
  let os, dec, info = init () in
  (* TODO: handle more formats *)
  let _ = assert (info.Theora.pixel_fmt = Theora.PF_420) in
  object (self)
    method width = info.Theora.frame_width
    method height = info.Theora.frame_height

    method frame_rate =
      float_of_int info.Theora.fps_numerator
      /. float_of_int info.Theora.fps_denominator

    val mutable latest_yuv = None

    method private get_yuv =
      try
        let yuv = Theora.Decoder.get_yuv dec os in
        let yuv =
          Theora.(
            Image.YUV420.make yuv.y_width yuv.y_height yuv.y yuv.y_stride yuv.u
              yuv.Theora.v yuv.u_stride)
        in
        latest_yuv <- Some yuv;
        yuv
      with
        | Ogg.Not_enough_data when not (Ogg.Stream.eos os) ->
            fill os;
            self#get_yuv
        | Theora.Duplicate_frame -> (
            (* Got a duplicate frame, sending previous one ! *)
              match latest_yuv with
              | Some x -> x
              | None -> raise Video.IO.Invalid_file)

    method read buf ofs len =
      let n = ref 0 in
      try
        while !n < len do
          buf.(ofs + !n) <- self#get_yuv;
          incr n
        done;
        !n
      with Ogg.Not_enough_data -> !n

    method close = Unix.close fd
  end
