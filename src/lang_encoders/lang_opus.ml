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

open Lang_values
open Lang_encoders

let make params =
  let defaults =
    {
      Opus_format.application= None;
      complexity= None;
      max_bandwidth= None;
      mode= Opus_format.VBR true;
      bitrate= `Auto;
      fill= None;
      channels= 2;
      samplerate= 48000;
      signal= None;
      frame_size= 20.;
      dtx= false;
      phase_inversion= true;
    }
  in
  let opus =
    List.fold_left
      (fun f -> function "application", {term= String "voip"; _} ->
            {f with Opus_format.application= Some `Voip}
        | "application", {term= String "audio"; _} ->
            {f with Opus_format.application= Some `Audio}
        | "application", {term= String "restricted_lowdelay"; _} ->
            {f with Opus_format.application= Some `Restricted_lowdelay}
        | "complexity", ({term= Int c; _} as t) ->
            (* Doc say this should be from 0 to 10. *)
            if c < 0 || c > 10 then
              raise (Error (t, "Opus complexity should be in 0..10")) ;
            {f with Opus_format.complexity= Some c}
        | "max_bandwidth", {term= String "narrow_band"; _} ->
            {f with Opus_format.max_bandwidth= Some `Narrow_band}
        | "max_bandwidth", {term= String "medium_band"; _} ->
            {f with Opus_format.max_bandwidth= Some `Medium_band}
        | "max_bandwidth", {term= String "wide_band"; _} ->
            {f with Opus_format.max_bandwidth= Some `Wide_band}
        | "max_bandwidth", {term= String "super_wide_band"; _} ->
            {f with Opus_format.max_bandwidth= Some `Super_wide_band}
        | "max_bandwidth", {term= String "full_band"; _} ->
            {f with Opus_format.max_bandwidth= Some `Full_band}
        | "frame_size", ({term= Float size; _} as t) ->
            let frame_sizes = [2.5; 5.; 10.; 20.; 40.; 60.] in
            if not (List.mem size frame_sizes) then
              raise
                (Error
                   ( t,
                     "Opus frame size should be one of 2.5, 5., 10., 20., 40. \
                      or 60." )) ;
            {f with Opus_format.frame_size= size}
        | "samplerate", ({term= Int i; _} as t) ->
            let samplerates = [8000; 12000; 16000; 24000; 48000] in
            if not (List.mem i samplerates) then
              raise
                (Error
                   ( t,
                     "Opus samplerate should be one of 8000, 12000, 16000, \
                      24000 or 48000" )) ;
            {f with Opus_format.samplerate= i}
        | "bitrate", ({term= Int i; _} as t) ->
            let i = i * 1000 in
            (* Doc say this should be from 500 to 512000. *)
            if i < 500 || i > 512000 then
              raise (Error (t, "Opus bitrate should be in 5..512")) ;
            {f with Opus_format.bitrate= `Bitrate i}
        | "bitrate", {term= String "auto"; _} ->
            {f with Opus_format.bitrate= `Auto}
        | "bitrate", {term= String "max"; _} ->
            {f with Opus_format.bitrate= `Bitrate_max}
        | "channels", ({term= Int i; _} as t) ->
            if i < 1 || i > 2 then
              raise
                (Error (t, "only mono and stereo streams are supported for now")) ;
            {f with Opus_format.channels= i}
        | "vbr", {term= String "none"; _} ->
            {f with Opus_format.mode= Opus_format.CBR}
        | "vbr", {term= String "constrained"; _} ->
            {f with Opus_format.mode= Opus_format.VBR true}
        | "vbr", {term= String "unconstrained"; _} ->
            {f with Opus_format.mode= Opus_format.VBR false}
        | "signal", {term= String "voice"; _} ->
            {f with Opus_format.signal= Some `Voice}
        | "signal", {term= String "music"; _} ->
            {f with Opus_format.signal= Some `Music}
        | "bytes_per_page", {term= Int i; _} ->
            {f with Opus_format.fill= Some i} | "dtx", {term= Bool b; _} ->
            {f with Opus_format.dtx= b}
        | "phase_inversion", {term= Bool b; _} ->
            {f with Opus_format.phase_inversion= b}
        | "", {term= Var s; _} when String.lowercase_ascii s = "mono" ->
            {f with Opus_format.channels= 1}
        | "", {term= Var s; _} when String.lowercase_ascii s = "stereo" ->
            {f with Opus_format.channels= 2} | _, t -> raise (generic_error t))
      defaults params
  in
  Ogg_format.Opus opus
