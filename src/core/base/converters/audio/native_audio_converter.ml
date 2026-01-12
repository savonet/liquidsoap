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

open Mm

(** Native audio converters *)

let samplerate_conf =
  Dtools.Conf.void
    ~p:(Audio_converter.Samplerate.samplerate_conf#plug "native")
    "Native samplerate conversion settings"
    ~comments:["Options related to native samplerate conversion."]

let quality_conf =
  Dtools.Conf.string
    ~p:(samplerate_conf#plug "quality")
    "Resampling quality" ~d:"linear"
    ~comments:["Resampling quality: either \"nearest\" or \"linear\"."]

let quality_of_string = function
  | "nearest" -> `Nearest
  | "linear" -> `Linear
  | s ->
      raise
        (Error.Invalid_value
           ( Lang.string s,
             "Native resampling quality must either be \"nearest\" or \
              \"linear\"." ))

let samplerate_converter _ =
  let mode = quality_of_string quality_conf#get in
  fun r data ofs len ->
    if r = 1. then (data, ofs, len)
    else (
      let data = Audio.resample ~mode r data ofs len in
      (data, 0, Audio.length data))

let () =
  Plug.register Audio_converter.Samplerate.converters "native"
    ~doc:
      "Native samplerate converter. This is fast but bad quality: you should \
       avoid using it for now if you are serious about sound."
    samplerate_converter

let channel_layout_converter src dst =
  assert (src <> dst);
  match dst with
    | `Mono -> fun data -> [| Audio.to_mono data 0 (Audio.length data) |]
    | `Stereo -> fun data -> [| data.(0); data.(0) |]
    | _ -> raise Audio_converter.Channel_layout.Unsupported

let () =
  Plug.register Audio_converter.Channel_layout.converters "native"
    ~doc:"Native channel layout converter." channel_layout_converter
