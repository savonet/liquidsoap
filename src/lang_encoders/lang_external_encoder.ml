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
      External_encoder_format.channels
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *)=
        2;
      samplerate= Frame.audio_rate;
      video= false;
      header= true;
      restart_on_crash= false;
      restart= External_encoder_format.No_condition;
      process= "";
    }
  in
  let ext =
    List.fold_left
      (fun f -> function "channels", {term= Int c; _} ->
            {f with External_encoder_format.channels= c}
        | "samplerate", {term= Int i; _} ->
            {f with External_encoder_format.samplerate= Lazy.from_val i}
        | "video", {term= Bool h; _} ->
            {f with External_encoder_format.video= h}
        | "header", {term= Bool h; _} ->
            {f with External_encoder_format.header= h}
        | "restart_on_crash", {term= Bool h; _} ->
            {f with External_encoder_format.restart_on_crash= h}
        | "", {term= Var s; _}
          when String.lowercase_ascii s = "restart_on_metadata" ->
            {
              f with
              External_encoder_format.restart= External_encoder_format.Metadata;
            } | "restart_after_delay", {term= Int i; _} ->
            {
              f with
              External_encoder_format.restart= External_encoder_format.Delay i;
            } | "process", {term= String s; _} ->
            {f with External_encoder_format.process= s}
        | "", {term= String s; _} ->
            {f with External_encoder_format.process= s} | _, t ->
            raise (generic_error t))
      defaults params
  in
  if ext.External_encoder_format.process = "" then
    raise External_encoder_format.No_process ;
  Encoder.External ext
