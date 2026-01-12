(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
open Mm

class mean ~field ~normalize source =
  object
    inherit operator [source] ~name:"mean"

    inherit
      Conversion.base
        ~converter:(fun frame ->
          (* Compute the mean of audio channels *)
          let len = Frame.position frame in
          let alen = Frame.audio_of_main len in
          let src_content = Content.Audio.get_data (Frame.get frame field) in
          let dst_content = Audio.Mono.create alen in
          let amp =
            if normalize then 1. /. float (Array.length src_content) else 1.
          in
          for i = 0 to alen - 1 do
            dst_content.(i) <-
              Array.fold_left (fun m b -> m +. b.(i)) 0. src_content *. amp
          done;
          Frame.set_data frame field Content.Audio.lift_data [| dst_content |])
        source
  end

let _ =
  let in_t = Format_type.audio () in
  let out_t = Format_type.audio_mono () in
  Lang.add_track_operator ~base:Modules.track_audio "mean"
    [
      ( "normalize",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Divide the output volume by the number of channels." );
      ("", in_t, None, Some "Track whose mean should be computed.");
    ]
    ~return_t:out_t ~category:`Conversion
    ~descr:"Produce mono audio by taking the mean of all audio channels."
    (fun p ->
      let normalize = Lang.to_bool (List.assoc "normalize" p) in
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      (field, new mean ~field ~normalize s))
