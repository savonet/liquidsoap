(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

 (** External audio conversion utilities *)

let log = Dtools.Log.make ["audio";"converter"]

(** TODO: is it the right place for this ? *)
let audio_conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "audio") "Audio settings"
    ~comments:["Options related to audio."]

let converter_conf =
  Dtools.Conf.void ~p:(audio_conf#plug "converter") "Conversion settings"
    ~comments:["Options related to audio conversion."]

module Samplerate=
struct

  exception Invalid_data

  type converter = float -> float array -> int -> int -> float array

  type converter_plug = unit -> converter

  type t = converter array

  let samplerate_conf =
    Dtools.Conf.void ~p:(converter_conf#plug "samplerate")
      "Samplerate conversion settings"
      ~comments:["Options related to samplerate conversion."]

  let preferred_conf =
    Dtools.Conf.string ~p:(samplerate_conf#plug "preferred")
      "Preferred samplerate converter"
      ~d:"libsamplerate" ~comments:["Preferred samplerate converter."]

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

  let resample conv ratio data ofs len =
    if Array.length conv <> Array.length data then
      raise Invalid_data;
    let convert i b =
      if ratio = 1. then
        Array.sub b ofs len
      else
        conv.(i) ratio b ofs len
    in
    if Array.length data.(0) = 0 then
      data
    else
      Array.mapi convert data

  (** Log which converter is used at start. *)
  let () =
    ignore (Dtools.Init.at_start
      (fun () ->
         let preferred = preferred_conf#get in
         match converters#get preferred with
           | Some v ->
              log#f 4 "Using preferred samplerate converter: %s." preferred;
           | None ->
              log#f 4 "Couldn't find preferred samplerate converter: %s."
                preferred;
              let (n,_) = List.hd converters#get_all in
              log#f 4 "Using %s samplerate converter" n))

end
