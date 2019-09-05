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

(** External audio samplerate conversion utilities. *)

let log = Log.make ["audio";"converter"]

(* TODO: is it the right place for this ? *)
let audio_conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "audio") "Audio settings"
    ~comments:["Options related to audio."]

let converter_conf =
  Dtools.Conf.void ~p:(audio_conf#plug "converter") "Conversion settings"
    ~comments:["Options related to audio conversion."]

module Samplerate=
struct

  exception Invalid_data

  (** A converter takes a convertion ratio (output samplerate / input
      samplerate), an audio buffer, and returns a resampled buffer. *)
  type converter = float -> Frame.audio_t -> Frame.audio_t

  type converter_plug = unit -> converter

  type t = converter array

  let samplerate_conf =
    Dtools.Conf.void ~p:(converter_conf#plug "samplerate")
      "Samplerate conversion settings"
      ~comments:["Options related to samplerate conversion."]

  let preferred_conf =
    Dtools.Conf.string ~p:(samplerate_conf#plug "preferred")
      "Preferred samplerate converter"
      ~d:"libsamplerate"
      ~comments:["Preferred samplerate converter. The native converter is always \
                  available by setting this to \"native\"."]

  let converters : converter_plug Plug.plug =
    Plug.create "samplerate converters"
      ~doc:"Methods for converting samplerate."

  let create channels =
    let preferred = preferred_conf#get in
    match converters#get preferred with
    | Some v ->
      Array.init channels (fun _ -> v ())
    | None ->
         (* List should never be empty, since at least
          * the native converter is available.. *)
         let (_,v) = List.hd converters#get_all in
         Array.init channels (fun _ -> v ())

  let resample conv ratio data =
    if Array.length conv <> Array.length data then raise Invalid_data;
    let convert i b =
      if ratio = 1. then b
      else conv.(i) ratio b
    in
    Array.mapi convert data

  (** Log which converter is used at start. *)
  let () =
    ignore (Dtools.Init.at_start
      (fun () ->
         let preferred = preferred_conf#get in
         match converters#get preferred with
           | Some _ ->
              log#important "Using preferred samplerate converter: %s." preferred;
           | None ->
              log#important "Couldn't find preferred samplerate converter: %s."
                preferred;
              let (n,_) = List.hd converters#get_all in
              log#important "Using %s samplerate converter" n))

end
