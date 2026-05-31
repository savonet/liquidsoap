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
open Mm_audio

module OSS = struct
  external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"

  external set_channels : Unix.file_descr -> int -> int
    = "caml_oss_dsp_channels"

  external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"
end

(* TODO: other formats than 16 bits? *)
class writer ?(device = "/dev/dsp") channels sample_rate =
  object (self)
    inherit IO.Unix.rw ~write:true device

    initializer
      assert (OSS.set_format fd 16 = 16);
      assert (OSS.set_channels fd channels = channels);
      assert (OSS.set_rate fd sample_rate = sample_rate)

    method private stream_really_write buf ofs len =
      let w = ref 0 in
      while !w <> len do
        w := !w + self#stream_write buf (ofs + !w) (len - !w)
      done

    method write buf ofs len =
      let s = Audio.S16LE.make buf ofs len in
      self#stream_really_write s 0 (String.length s)

    method close = self#stream_close
  end

class reader ?(device = "/dev/dsp") channels sample_rate =
  object (self)
    inherit IO.Unix.rw ~read:true device

    initializer
      assert (OSS.set_format fd 16 = 16);
      assert (OSS.set_channels fd channels = channels);
      assert (OSS.set_rate fd sample_rate = sample_rate)

    method channels = channels
    method sample_rate = sample_rate
    method length : int = assert false
    method duration : float = assert false

    method read buf ofs len =
      let slen = Audio.S16LE.length channels len in
      let s = Bytes.create slen in
      let r = self#stream_read s 0 slen in
      let len = Audio.S16LE.length channels r in
      Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf ofs len;
      len

    method seek (_ : int) : unit = assert false
    method close = self#stream_close
  end
