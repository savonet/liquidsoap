(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang_values
open Lang_encoders

let make_cbr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.CBR 128 ;
        channels = 2 ;
        fill = None;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.CBR i }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let make_abr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.ABR (None,None,None) ;
        channels = 2 ;
        fill = None ;
        samplerate = 44100 ;
    }
  in
  let get_rates x =
    match x.Encoder.Vorbis.mode with
      | Encoder.Vorbis.ABR (x,y,z) -> x,y,z
      | _ -> assert false
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              let (x,_,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,Some i,y) }
          | ("max_bitrate",{ term = Int i; _}) ->
              let (x,y,_) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,y,Some i) }
          | ("min_bitrate",{ term = Int i; _}) ->
              let (_,x,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (Some i,x,y) }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let make params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.VBR 0.3 ;
        channels = 2 ;
        fill = None ;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("quality",({ term = Float q; _} as t)) ->
              if q<(-0.2) || q>1. then
                raise (Error (t,"quality should be in [(-0.2)..1]")) ;
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("quality",({ term = Int i; _} as t)) ->
              if i<>0 && i<>1 then
                raise (Error (t,"quality should be in [-(0.2)..1]")) ;
              let q = float i in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis
