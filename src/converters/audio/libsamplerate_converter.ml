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

(** Samplerate converter using libsamplerate *)

let log = Log.make ["audio"; "converter"; "libsamplerate"]

let samplerate_conf =
  Dtools.Conf.void
    ~p:(Audio_converter.Samplerate.samplerate_conf#plug "libsamplerate")
    "Libsamplerate conversion settings"
    ~comments:["Options related to libsamplerate conversion."]

let quality_conf =
  Dtools.Conf.string
    ~p:(samplerate_conf#plug "quality")
    "Resampling quality" ~d:"fast"
    ~comments:
      [
        "Resampling quality, one of: `\"best\"`, `\"medium\"`, `\"fast\"`, \
         `\"zero_order\"` or `\"linear\"`. Refer to ocaml-samplerate for \
         details.";
      ]

let quality_of_string v =
  match v with
    | "best" -> Samplerate.Conv_sinc_best_quality
    | "medium" -> Samplerate.Conv_sinc_medium_quality
    | "fast" -> Samplerate.Conv_fastest
    | "zero_order" -> Samplerate.Conv_zero_order_hold
    | "linear" -> Samplerate.Conv_linear
    | _ ->
        raise
          (Lang_errors.Invalid_value
             ( Lang.string v,
               "libsamplerate quality must be one of: \"best\", \"medium\", \
                \"fast\", \"zero_order\", \"linear\"." ))

let samplerate_converter () =
  let quality = quality_of_string quality_conf#get in
  let converter = Samplerate.create quality 1 in
  let convert ratio b =
    let inlen = Audio.Mono.length b in
    let outlen = int_of_float (float inlen *. ratio) in
    let buf = Audio.Mono.create outlen in
    let i, o = Samplerate.process_ba converter ratio b buf in
    if i < inlen then
      log#debug "Could not convert all the input buffer (%d instead of %d)." i
        inlen;
    if o < outlen then
      log#debug "Unexpected output length (%d instead of %d)." o outlen;

    (* TODO: the following would solve the issue but apparently messes up buffers *)
    (* Audio.Mono.sub buf 0 o *)
    buf
  in
  convert

let () =
  Audio_converter.Samplerate.converters#register "libsamplerate"
    samplerate_converter
