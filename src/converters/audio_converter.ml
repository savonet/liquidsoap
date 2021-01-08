(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** External audio samplerate conversion utilities. *)

let log = Log.make ["audio"; "converter"]

(* TODO: is it the right place for this ? *)
let audio_conf =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "audio")
    "Audio settings"
    ~comments:["Options related to audio."]

let converter_conf =
  Dtools.Conf.void
    ~p:(audio_conf#plug "converter")
    "Conversion settings"
    ~comments:["Options related to audio conversion."]

module Samplerate = struct
  exception Invalid_data

  (** A converter takes a convertion ratio (output samplerate / input
      samplerate), an audio buffer, and returns a resampled buffer. *)
  type converter = float -> Audio.Mono.buffer -> Audio.Mono.buffer

  type converter_plug = unit -> converter
  type t = converter array

  let samplerate_conf =
    Dtools.Conf.void
      ~p:(converter_conf#plug "samplerate")
      "Samplerate conversion settings"
      ~comments:["Options related to samplerate conversion."]

  let converters_conf =
    Dtools.Conf.list
      ~p:(samplerate_conf#plug "converters")
      "Preferred samplerate converters"
      ~d:["ffmpeg"; "libsamplerate"; "native"]
      ~comments:
        [
          "Preferred samplerate converter. The native converter is always \
           available.";
        ]

  let converters : converter_plug Plug.plug =
    Plug.create "samplerate converters"
      ~doc:"Methods for converting samplerate."

  let create channels =
    let converter =
      let rec f = function
        | conv :: l -> (
            match converters#get conv with Some v -> v | None -> f l )
        | [] ->
            (* This should never come up since the native converter is always
               available. *)
            assert false
      in
      f converters_conf#get
    in
    Array.init channels (fun _ -> converter ())

  let resample conv ratio data =
    if Array.length conv <> Array.length data then raise Invalid_data;
    let convert i b = if ratio = 1. then b else conv.(i) ratio b in
    Array.mapi convert data

  (** Log which converter is used at start. *)
  let () =
    Lifecycle.before_start (fun () ->
        let rec f = function
          | conv :: _ when converters#get conv <> None ->
              log#important "Using samplerate converter: %s." conv
          | _ :: l -> f l
          | [] -> assert false
        in
        f converters_conf#get)
end

module Channel_layout = struct
  exception Unsupported
  exception Invalid_data

  type layout = [ `Mono | `Stereo | `Five_point_one ]

  type converter =
    layout -> layout -> Frame_content.Audio.data -> Frame_content.Audio.data

  type t = {
    src : layout;
    converter : Frame_content.Audio.data -> Frame_content.Audio.data;
  }

  let channel_layout_conf =
    Dtools.Conf.void
      ~p:(converter_conf#plug "channel_layout")
      "Channel layout conversion settings"
      ~comments:["Options related to channel layout conversion."]

  let converters_conf =
    Dtools.Conf.list
      ~p:(channel_layout_conf#plug "converters")
      "Preferred samplerate converters" ~d:["native"]
      ~comments:
        [
          "Preferred channel layout converter. The native converter is always \
           available.";
        ]

  let converters : converter Plug.plug =
    Plug.create "channel layout converters"
      ~doc:"Methods for converting channel layouts."

  let create src dst =
    let converter =
      if src = dst then fun _ _ x -> x
      else (
        let rec f = function
          | conv :: l -> (
              match converters#get conv with Some v -> v | None -> f l )
          | [] ->
              (* This should never come up since the native converter is always
                 available. *)
              assert false
        in
        f converters_conf#get )
    in
    { src; converter = converter src dst }

  let channels_of_layout = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let layout_of_channels = function
    | 1 -> `Mono
    | 2 -> `Stereo
    | 6 -> `Five_point_one
    | _ -> raise Unsupported

  let convert { src; converter } input =
    if Array.length input != channels_of_layout src then raise Invalid_data;
    converter input
end
