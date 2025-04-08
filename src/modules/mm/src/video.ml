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

open Mm_base

(* open Mm_image *)
open Mm_audio
module YUV420 = Mm_image.Image.YUV420

(** Images from which are made videos. *)
module type Image = sig
  type t

  val create : int -> int -> t
  val size : t -> int
  val blit_all : t -> t -> unit
  val copy : t -> t
  val blank : t -> unit
  val randomize : t -> unit
end

module Image = struct
  include Mm_image.Image.YUV420

  let create w h = create w h
  let scale = scale ~proportional:false
end

module Make (Image : Image) = struct
  module I = Image

  type t = Image.t array
  type buffer = t

  let make len width height =
    Array.init len (fun _ -> Image.create width height)

  let single img = [| img |]

  let blit sbuf sofs dbuf dofs len =
    for i = 0 to len - 1 do
      Image.blit_all sbuf.(sofs + i) dbuf.(dofs + i)
    done

  let copy vid = Array.map Image.copy vid
  let length vid = Array.length vid

  let size vid =
    let n = ref 0 in
    for i = 0 to Array.length vid - 1 do
      n := !n + Image.size vid.(i)
    done;
    !n

  let get vid i = vid.(i)
  let set vid i img = vid.(i) <- img

  let iter f vid off len =
    for i = off to off + len - 1 do
      f vid.(i)
    done

  let blank vid off len = iter Image.blank vid off len
  let randomize vid off len = iter Image.randomize vid off len
end

include Make (Image)

(* Canvas are not in place so that we have to make a slightly different
   implementation. *)
module MakeCanvas (BaseImage : Mm_image.Image.CanvasImage) = struct
  module Image = Mm_image.Image.Canvas (Image)

  type image = Image.t
  type t = Image.t array

  let make len (width, height) : t =
    Array.init len (fun _ -> Image.create width height)

  let single img = [| img |]
  let single_image img = single (Image.make img)
  let length (v : t) = Array.length v
  let copy (v : t) = Array.init (length v) (fun i -> v.(i))

  let size (v : t) =
    let n = ref 0 in
    for i = 0 to Array.length v - 1 do
      n := !n + Image.size v.(i)
    done;
    !n

  let get v i = v.(i)
  let set v i img = v.(i) <- img
  let map_image f v i = v.(i) <- f v.(i)
  let render ?transparent v i = Image.render ?transparent v.(i)
  let put v i img = v.(i) <- Image.make img

  let blit sbuf sofs dbuf dofs len =
    for i = 0 to len - 1 do
      dbuf.(dofs + i) <- sbuf.(sofs + i)
    done

  let map f buf ofs len =
    for i = ofs to ofs + len - 1 do
      buf.(i) <- f buf.(i)
    done

  let blank buf ofs len =
    map
      (fun img -> Image.create (Image.width img) (Image.height img))
      buf ofs len

  let iter f buf ofs len =
    for i = ofs to ofs + len - 1 do
      buf.(i) <- Image.iter f buf.(i)
    done
end

module Canvas = MakeCanvas (Image)

(*
module RE = struct
  type t = Image.t

  let create () = Image.create 0 0

  let blit = blit
end
*)

(* module Ringbuffer_ext = Ringbuffer.Make_ext (RE) *)

(* module Ringbuffer = Ringbuffer.Make (RE) *)

module FPS = struct
  type t = float

  (* TODO: improve this! *)
  let to_frac f =
    let n = floor ((f *. 100.) +. 0.5) in
    let n = int_of_float n in
    if n mod 100 = 0 then (n / 100, 1) else (n, 100)
end

module AVI = struct
  module Writer = struct
    let word n =
      let s = Bytes.create 2 in
      Bytes.set_int16_le s 0 n;
      Bytes.unsafe_to_string s

    let dword n =
      let s = Bytes.create 4 in
      Bytes.set_int32_le s 0 (Int32.of_int n);
      Bytes.unsafe_to_string s

    module Chunk = struct
      let create id len =
        let pad = len mod 2 = 1 in
        assert (String.length id = 4);
        let s = Bytes.create (8 + len + if pad then 1 else 0) in
        Bytes.blit_string id 0 s 0 4;
        Bytes.blit_string (dword len) 0 s 4 4;
        if pad then Bytes.set s (8 + len) (char_of_int 0);
        s

      let make id data =
        let len = String.length data in
        let s = create id len in
        Bytes.blit_string data 0 s 8 len;
        Bytes.unsafe_to_string s

      (* Audio in 16LE *)
      (* let audio b = make "01wb" b *)

      let audio_s16le buf =
        let len = Audio.length buf in
        let channels = Audio.channels buf in
        let s = create "01wb" (len * channels * 2) in
        Audio.S16LE.of_audio buf 0 s 8 len;
        Bytes.unsafe_to_string s

      (* Video in RGB. *)
      (* let video b = make "00db" b *)

      let video_yuv420 img =
        let open Mm_image in
        let width = Image.YUV420.width img in
        let height = Image.YUV420.height img in
        let y, u, v = Image.YUV420.data img in
        let y = Image.Data.to_string y in
        let u = Image.Data.to_string u in
        let v = Image.Data.to_string v in
        let y_stride = Image.YUV420.y_stride img in
        let uv_stride = Image.YUV420.uv_stride img in
        let s =
          create "00db" ((width * height) + (2 * (width / 2) * (height / 2)))
        in
        let o = ref 8 in
        let add_sub data off len =
          Bytes.blit_string data off s !o len;
          o := !o + len
        in
        let add data = add_sub data 0 (String.length data) in
        if y_stride = width then add y
        else
          for j = 0 to height - 1 do
            add_sub y (j * y_stride) width
          done;
        if uv_stride = width / 2 then (
          add u;
          add v)
        else (
          for j = 0 to (height / 2) - 1 do
            add_sub u (j * uv_stride) (width / 2)
          done;
          for j = 0 to (height / 2) - 1 do
            add_sub v (j * uv_stride) (width / 2)
          done);
        Bytes.unsafe_to_string s

      let list = make "LIST"
    end

    let header ?(format = `YUV420) ~width ~height ~framerate ?channels
        ?samplerate ?vendor () =
      ignore format;
      let has_audio = channels <> None in
      let channels = Option.value ~default:0 channels in
      let samplerate = Option.value ~default:0 samplerate in
      assert ((not has_audio) || samplerate > 0);
      (* Writing in two steps because 0xffffffff cannot be represented on 32 bits
         architectures. *)
      let dword_max () = word 0xffff ^ word 0xffff in
      let avi_header =
        Chunk.make "avih"
          (dword (1000000 / framerate) (* microsec per frame *)
          ^ dword 0 (* maximum bytes per second *)
          ^ dword 0 (* reserved *)
          ^ dword 0x0100 (* flags (interleaved) *)
          ^ dword_max () (* number of frames *)
          ^ dword 0 (* initial frame *)
          ^ dword (1 + if has_audio then 1 else 0) (* number of streams *)
          ^ dword 0 (* suggested buffer size *)
          ^ dword width (* width *) ^ dword height (* height *)
          ^ dword 0 (* reserved *) ^ dword 0 (* reserved *)
          ^ dword 0 (* reserved *) ^ dword 0 (* reserved *))
      in
      let video_header =
        let stream_header =
          Chunk.make "strh"
            ("vids" (* stream type *) ^ "I420" (* fourcc (codec) *)
            ^ dword 0 (* flags *) ^ word 0
            (* priority *) ^ word 0 (* language *)
            ^ dword 0 (* initial frames *)
            ^ dword 1 (* scale *) ^ dword framerate (* rate *)
            ^ dword 0 (* start time *)
            ^ dword_max () (* stream length *)
            ^ dword 0 (* suggested buffer size *)
            ^ dword_max () (* quality *) ^ dword 0 (* sample size *)
            ^ word 0 (* left *) ^ word 0
            (* top *) ^ word width (* right *)
            ^ word height (* bottom *))
        in
        let stream_format =
          (* see BITMAPINFO *)
          Chunk.make "strf"
            (dword 40 (* size of this structure *)
            ^ dword width (* width *) ^ dword height (* height *)
            ^ word 1 (* panes *) ^ word 12
            (* depth *) ^ "I420" (* codec *)
            ^ dword (width * height * 6 / 4) (* image size *)
            ^ dword 0 (* pixels / x meter *)
            ^ dword 0 (* pixels / y meter *)
            ^ dword 0 (* colors used *)
            ^ dword 0 (* important colors *))
        in
        Chunk.list ("strl" ^ stream_header ^ stream_format)
      in
      let audio_header =
        if not has_audio then ""
        else (
          let stream_header =
            Chunk.make "strh"
              ("auds" (* stream type *) ^ dword 0 (* stream *)
              ^ dword 0 (* flags *) ^ word 0 (* priority *)
              ^ word 0 (* language *)
              ^ dword 0 (* initial frames *)
              ^ dword 1 (* scale *)
              ^ dword samplerate (* rate *)
              ^ dword 0 (* start time *)
              ^ dword_max () (* stream length *)
              ^ dword 0 (* suggested buffer size *)
              ^ dword_max () (* quality *)
              ^ dword (2 * channels) (* sample size *)
              ^ word 0 (* left *) ^ word 0
              (* top *) ^ word 0 (* right *)
              ^ word 0 (* bottom *))
          in
          let stream_format =
            Chunk.make "strf"
              (word 1 (* stream type (PCM) *)
              ^ word channels (* channels *)
              ^ dword samplerate (* rate *)
              ^ dword (2 * channels * samplerate) (* byte rate *)
              ^ word (2 * channels) (* block align *)
              ^ word 16 (* bits per sample *)
              ^ word 0 (* size of extra information *))
          in
          Chunk.list ("strl" ^ stream_header ^ stream_format))
      in
      let headers =
        Chunk.list ("hdrl" ^ avi_header ^ video_header ^ audio_header)
      in
      let info =
        match vendor with
          | Some vendor ->
              let producer = Chunk.make "ISFT" vendor in
              Chunk.list ("INFO" ^ producer)
          | None -> ""
      in
      "RIFF"
      ^ dword_max () (* file size *)
      ^ "AVI " ^ headers ^ info ^ "LIST" ^ dword_max () ^ "movi"
  end
end

module IO = struct
  exception Invalid_file

  module Reader = struct
    class type t = object
      method width : int
      method height : int
      method frame_rate : float

      (* method set_target_size : int -> int -> unit *)
      method read : buffer -> int -> int -> int

      (* method read_audio : Audio.buffer -> int -> int -> int *)
      method close : unit
    end
  end

  module Writer = struct
    class type t = object
      method write : buffer -> int -> int -> unit

      (* method write_audio : Audio.buffer -> int -> int -> unit *)
      method close : unit
    end

    class virtual avi frame_rate w h =
      (* let has_audio = audio_rate <> None in *)
      let frames_per_chunk = int_of_float (frame_rate +. 0.5) in
      let frame_size = w * h * 3 in
      object (self)
        inherit IO.helper
        method virtual private stream_write : string -> int -> int -> int
        method virtual private stream_seek : int -> unit
        method virtual private stream_close : unit

        initializer
          self#output "RIFF";
          self#output_int 0;
          (* TOFILL: file size *)
          self#output "AVI ";

          (* file type *)

          (* Headers *)
          self#output "LIST";
          self#output_int 192;
          (* size of the list *)
          self#output "hdrl";

          (* AVI header *)
          self#output "avih";
          self#output_int 56;
          (* AVI header size *)
          self#output_int (int_of_float (1000000. /. frame_rate));
          (* microseconds per frame *)
          self#output_int 0;
          (* max bytes per sec *)
          self#output_int 0;
          (* pad to multiples of this size *)
          self#output_byte 0;
          (* flags *)
          self#output_byte 1;
          (* flags (interleaved) *)
          self#output_byte 0;
          (* flags *)
          self#output_byte 0;
          (* flags *)
          self#output_int 0;
          (* TOFILL: total number of frames *)
          self#output_int 0;
          (* initial frame *)
          self#output_int 1;
          (* number of streams (TODO: change if audio) *)
          self#output_int 0;
          (* suggested buffer size *)
          self#output_int w;
          (* width *)
          self#output_int h;
          (* height *)
          self#output_int 0;
          (* scale *)
          self#output_int 0;
          (* rate *)
          self#output_int 0;
          (* start *)
          self#output_int 0;

          (* length *)

          (* Stream headers *)
          self#output "LIST";
          self#output_int 116;
          self#output "strl";

          (* Stream header *)
          self#output "strh";
          self#output_int 56;
          self#output "vids";
          self#output "RGB ";
          (* codec *)
          self#output_int 0;
          (* flags *)
          self#output_int 0;
          (* stream priority and language *)
          self#output_int 0;
          (* initial frames *)
          self#output_int 10;
          (* scale : rate / scale = frames / second or samples / second *)
          self#output_int (int_of_float (frame_rate *. 10.));
          (* rate *)
          self#output_int 0;
          (* stream start time (in frames). *)
          self#output_int 0;
          (* TOFILL: stream length (= number of frames) *)
          self#output_int (frames_per_chunk * frame_size);
          (* suggested buffer size *)
          self#output_int 0;
          (* stream quality *)
          self#output_int 0;
          (* size of samples *)
          self#output_short 0;
          (* destination rectangle: left *)
          self#output_short 0;
          (* top *)
          self#output_short w;
          (* right *)
          self#output_short h;

          (* bottom *)

          (* Stream format *)
          self#output "strf";
          self#output_int 40;
          self#output_int 40;
          (* video size (????) *)
          self#output_int w;
          (* width *)
          self#output_int h;
          (* height *)
          self#output_short 1;
          (* panes *)
          self#output_short 24;
          (* color depth *)
          self#output_int 0;
          (* tag1 (????) *)
          self#output_int frame_size;
          (* image size *)
          self#output_int 0;
          (* X pixels per meter *)
          self#output_int 0;
          (* Y pixels per meter *)
          self#output_int 0;
          (* colors used *)
          self#output_int 0;

          (* Important colors *)

          (* movie data *)
          self#output "LIST";
          self#output_int 0;
          (* TOFILL: movie size *)
          self#output "movi";

          (* video chunks follow *)
          self#output "00dc";
          self#output_int 0

        (* TOFILL: size *)
        val mutable datalen = 0
        val mutable dataframes = 0

        method write (_ : buffer) ofs len =
          for _ = ofs to ofs + len - 1 do
            (* let s = Image.to_RGB24_string buf.(i) in *)
            let s = failwith "TODO: output YUV420 avi" in
            self#output s;
            datalen <- datalen + String.length s
          done;
          dataframes <- dataframes + len

        method close =
          Printf.printf "completing... (%d frames)\n%!" dataframes;
          self#stream_seek 4;
          self#output_int (datalen + (56 * 4));
          self#stream_seek (12 * 4);
          self#output_int dataframes;
          self#stream_seek (35 * 4);
          self#output_int dataframes;
          self#stream_seek (54 * 4);
          self#output_int (datalen + (3 * 4));
          self#stream_seek (57 * 4);
          self#output_int datalen;
          self#stream_close
      end

    class to_avi_file fname fr w h =
      object
        inherit avi fr w h
        inherit IO.Unix.rw ~write:true fname
      end
  end
end
