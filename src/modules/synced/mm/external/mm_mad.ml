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

(* TODO: use optimized version for files. *)
class virtual reader =
  object (self)
    inherit IO.helper
    method virtual private stream_close : unit
    val mutable channels = 0
    method channels = channels

    (* TODO *)
    method length : int = failwith "TODO"
    method duration : float = failwith "TODO"
    method sample_rate = 44100
    val mutable rb = Audio.Ringbuffer_ext.create 0 0
    val mutable mf = None
    method private mf = match mf with Some mf -> mf | _ -> assert false

    initializer
      let f = Mad.openstream self#stream_read in
      (* let _, c, _ = Mad.get_output_format f in *)
      (* TODO: we should decode a frame in order to get the real number of
         channels... *)
      let c = 2 in
      mf <- Some f;
      channels <- c;
      rb <- Audio.Ringbuffer_ext.create channels 0

    method private decode = Mad.decode_frame_float self#mf
    method close = self#stream_close

    method read buf ofs len =
      let r = ref (-1) in
      while !r <> 0 && Audio.Ringbuffer_ext.read_space rb < len do
        let data =
          try self#decode
          with Mad.End_of_stream -> Audio.create self#channels 0
        in
        r := Audio.length data;
        Audio.Ringbuffer_ext.write rb data
      done;
      let maxlen = Audio.Ringbuffer_ext.read_space rb in
      let len = min maxlen len in
      Audio.Ringbuffer_ext.read rb (Audio.sub buf ofs len);
      len

    (* TODO *)
    method seek (_ : int) : unit = failwith "TODO"
  end

class reader_of_file fname =
  object
    inherit IO.Unix.rw ~read:true fname
    inherit reader
  end
