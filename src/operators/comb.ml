(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Source

(* See http://en.wikipedia.org/wiki/Comb_filter *)

class comb ~kind (source : source) delay feedback =
  let past_len = Frame.audio_of_seconds delay in
  let channels = AFrame.channels_of_kind kind in
  object
    inherit operator ~name:"comb" kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    val past = Audio.make channels past_len 0.

    val mutable past_pos = 0

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf in
      let position = AFrame.position buf in
      let feedback = feedback () in
      for i = offset to position - 1 do
        for c = 0 to Array.length b - 1 do
          let oldin = b.(c).{i} in
          b.(c).{i} <- b.(c).{i} +. (past.(c).{past_pos} *. feedback);
          past.(c).{past_pos} <- oldin
        done;
        past_pos <- (past_pos + 1) mod past_len
      done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "comb"
    [
      ("delay", Lang.float_t, Some (Lang.float 0.001), Some "Delay in seconds.");
      ( "feedback",
        Lang.float_getter_t (),
        Some (Lang.float (-6.)),
        Some "Feedback coefficient in dB." );
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Comb filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let duration, feedback, src =
        ( Lang.to_float (f "delay"),
          Lang.to_float_getter (f "feedback"),
          Lang.to_source (f "") )
      in
      new comb ~kind src duration (fun () -> Audio.lin_of_dB (feedback ())))
