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

exception Invalid

module Contents = struct
  type format = ..
  type kind = ..
  type data = ..
end

module type ContentSpecs = sig
  type kind
  type param
  type data

  val make : param list -> data
  val blit : data -> int -> data -> int -> int -> unit
  val bytes : data -> int
  val copy : data -> data
  val params : data -> param list
  val merge : param list -> param list -> param list
  val string_of_param : param -> string
  val param_of_string : string -> string -> param option
  val kind : kind
  val default_params : kind -> param list
  val string_of_kind : kind -> string
  val kind_of_string : string -> kind option
end

module type Content = sig
  include ContentSpecs

  val is_data : Contents.data -> bool
  val lift_data : data -> Contents.data
  val get_data : Contents.data -> data
  val is_format : Contents.format -> bool
  val lift_params : param list -> Contents.format
  val get_params : Contents.format -> param list
  val is_kind : Contents.kind -> bool
  val lift_kind : kind -> Contents.kind
  val get_kind : Contents.kind -> kind
end

type data = Contents.data
type kind = Contents.kind

type kind_handler = {
  default_format : unit -> Contents.format;
  string_of_kind : unit -> string;
}

let kind_handlers = Queue.create ()
let register_kind_handler fn = Queue.add fn kind_handlers

exception Found_kind of kind_handler

let get_kind_handler f =
  try
    Queue.iter
      (fun fn -> match fn f with Some h -> raise (Found_kind h) | None -> ())
      kind_handlers;
    raise Invalid
  with Found_kind h -> h

let kind_parsers = Queue.create ()

exception Parsed_kind of kind

let kind_of_string s =
  try
    Queue.iter
      (fun fn -> match fn s with Some f -> raise (Parsed_kind f) | None -> ())
      kind_parsers;
    raise Invalid
  with Parsed_kind f -> f

type format = Contents.format

type format_handler = {
  kind : unit -> kind;
  make : unit -> data;
  string_of_format : unit -> string;
  merge : format -> unit;
}

let format_handlers = Queue.create ()
let register_format_handler fn = Queue.add fn format_handlers

exception Found_format of format_handler

let get_params_handler p =
  try
    Queue.iter
      (fun fn ->
        match fn p with Some h -> raise (Found_format h) | None -> ())
      format_handlers;
    raise Invalid
  with Found_format h -> h

let format_parsers = Queue.create ()

exception Parsed_format of format

let format_of_param label value =
  try
    Queue.iter
      (fun fn ->
        match fn label value with
          | Some p -> raise (Parsed_format p)
          | None -> ())
      format_parsers;
    raise Invalid
  with Parsed_format p -> p

type data_handler = {
  blit : int -> data -> int -> int -> unit;
  bytes : unit -> int;
  copy : unit -> data;
  format : unit -> format;
}

let data_handlers = Queue.create ()
let register_data_handler fn = Queue.add fn data_handlers

exception Found_data of data_handler

let get_data_handler v =
  try
    Queue.iter
      (fun fn -> match fn v with Some h -> raise (Found_data h) | None -> ())
      data_handlers;
    raise Invalid
  with Found_data h -> h

let make k = (get_params_handler k).make ()

let blit src src_ofs dst dst_ofs len =
  let src = get_data_handler src in
  src.blit src_ofs dst dst_ofs len

let bytes c = (get_data_handler c).bytes ()
let copy c = (get_data_handler c).copy ()
let format c = (get_data_handler c).format ()
let kind p = (get_params_handler p).kind ()
let default_format f = (get_kind_handler f).default_format ()
let merge p p' = (get_params_handler p).merge p'
let string_of_kind f = (get_kind_handler f).string_of_kind ()
let string_of_format k = (get_params_handler k).string_of_format ()

module MkContent (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type param = C.param
     and type data = C.data = struct
  type Contents.kind += Kind of C.kind
  type Contents.format += Format of C.param list Unifier.t
  type Contents.data += Data of C.data

  let blit src src_ofs dst dst_ofs len =
    let dst = match dst with Data dst -> dst | _ -> raise Invalid in
    C.blit src src_ofs dst dst_ofs len

  let merge l l' =
    let l' = match l' with Format l' -> l' | _ -> raise Invalid in
    let u = List.sort_uniq compare in
    let m = C.merge (u (Unifier.deref l)) (u (Unifier.deref l')) in
    Unifier.set l' m;
    Unifier.(l <-- l')

  let kind_of_string s = Utils.maybe (fun p -> Kind p) (C.kind_of_string s)

  let format_of_string label value =
    Utils.maybe
      (fun p -> Format (Unifier.make [p]))
      (C.param_of_string label value)

  let () =
    register_kind_handler (function
      | Kind f ->
          Some
            {
              default_format =
                (fun () -> Format (Unifier.make (C.default_params f)));
              string_of_kind = (fun () -> C.string_of_kind f);
            }
      | _ -> None);
    register_format_handler (function
      | Format l ->
          Some
            {
              kind = (fun () -> Kind C.kind);
              make = (fun () -> Data (C.make (Unifier.deref l)));
              merge = (fun l' -> merge l l');
              string_of_format =
                (fun () ->
                  let kind = C.string_of_kind C.kind in
                  let params =
                    String.concat ","
                      (List.map C.string_of_param (Unifier.deref l))
                  in
                  match params with
                    | "" -> C.string_of_kind C.kind
                    | _ -> Printf.sprintf "%s(%s)" kind params);
            }
      | _ -> None);
    Queue.push kind_of_string kind_parsers;
    Queue.push format_of_string format_parsers;
    register_data_handler (function
      | Data d ->
          Some
            {
              blit = blit d;
              copy = (fun () -> Data (C.copy d));
              bytes = (fun () -> C.bytes d);
              format = (fun () -> Format (Unifier.make (C.params d)));
            }
      | _ -> None)

  let is_kind = function Kind _ -> true | _ -> false
  let lift_kind f = Kind f
  let get_kind = function Kind f -> f | _ -> raise Invalid
  let is_format = function Format _ -> true | _ -> false
  let lift_params p = Format (Unifier.make p)
  let get_params = function Format p -> Unifier.deref p | _ -> raise Invalid
  let is_data = function Data _ -> true | _ -> false
  let lift_data d = Data d
  let get_data = function Data d -> d | _ -> raise Invalid

  include C
end

module NoneSpecs = struct
  type kind = unit
  type param
  type data = unit

  let make _ = ()
  let blit _ _ _ _ _ = ()
  let bytes _ = 0
  let copy _ = ()
  let params _ = []
  let merge _ _ = []
  let string_of_param _ = assert false
  let param_of_string _ _ = None
  let kind = ()
  let default_params () = []
  let string_of_kind () = "none"
  let kind_of_string = function "none" -> Some () | _ -> None
end

module None = struct
  include MkContent (NoneSpecs)

  let data = lift_data ()
  let format = lift_params []
end

module AudioSpecs = struct
  open Frame_settings

  type kind = [ `Pcm ]
  type param = [ `Mono | `Stereo | `Five_point_one ]
  type data = Audio.Mono.buffer array

  let string_of_kind = function `Pcm -> "pcm"

  let string_of_param = function
    | `Mono -> "mono"
    | `Stereo -> "stereo"
    | `Five_point_one -> "dolby 5.1"

  let merge l l' =
    match (l, l') with
      | [], [] -> []
      | [x], [y] when x = y -> [x]
      | _ -> raise Invalid

  let bytes d =
    let float_bytes = 8 in
    let track_bytes t = Audio.Mono.length t * float_bytes in
    Array.fold_left (fun n t -> n + track_bytes t) 0 d

  let blit src src_pos dst dst_pos len =
    Array.iter2
      (fun a a' ->
        let ( ! ) = audio_of_master in
        Audio.Mono.blit
          (Audio.Mono.sub a !src_pos !len)
          (Audio.Mono.sub a' !dst_pos !len))
      src dst

  let copy d = Array.map Audio.Mono.copy d

  let param_of_channels = function
    | 1 -> `Mono
    | 2 -> `Stereo
    | 6 -> `Five_point_one
    | _ -> raise Invalid

  let channels_of_param = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let param_of_string label value =
    match (label, value) with
      | "", "mono" -> Some `Mono
      | "", "stereo" -> Some `Stereo
      | "", "5.1" -> Some `Five_point_one
      | _ -> None

  let params d = [param_of_channels (Array.length d)]
  let kind = `Pcm

  let default_params _ =
    [param_of_channels (Lazy.force Frame_settings.audio_channels)]

  let make l =
    let channels =
      match l with
        | [] -> Lazy.force Frame_settings.audio_channels
        | [p] -> channels_of_param p
        | _ -> raise Invalid
    in
    Array.init channels (fun _ -> Audio.Mono.create (audio_of_master !!size))

  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None
end

module Audio = struct
  include MkContent (AudioSpecs)

  let format_of_channels = function
    | 1 -> lift_params [`Mono]
    | 2 -> lift_params [`Stereo]
    | 6 -> lift_params [`Five_point_one]
    | _ -> raise Invalid

  let channels_of_format p =
    match get_params p with
      | [] -> Lazy.force Frame_settings.audio_channels
      | [p] -> AudioSpecs.channels_of_param p
      | _ -> raise Invalid
end

module VideoSpecs = struct
  open Frame_settings

  type kind = [ `Yuv420p ]
  type param
  type data = Video.t

  let string_of_kind = function `Yuv420p -> "yuv420p"
  let make _ = Video.make (video_of_master !!size) !!video_width !!video_height
  let string_of_param _ = assert false
  let param_of_string _ _ = None
  let bytes = Video.size

  let merge l l' =
    match (l, l') with
      | [], [] -> []
      | [x], [y] when x = y -> [x]
      | _ -> raise Invalid

  let blit src src_pos dst dst_pos len =
    let ( ! ) = Frame_settings.video_of_master in
    Video.blit src !src_pos dst !dst_pos !len

  let copy = Video.copy
  let params _ = []
  let kind = `Yuv420p
  let default_params _ = []

  let kind_of_string = function
    | "yuv420p" | "video" -> Some `Yuv420p
    | _ -> None
end

module Video = MkContent (VideoSpecs)

module MidiSpecs = struct
  open Frame_settings

  type kind = [ `Midi ]
  type param = [ `Channels of int ]
  type data = MIDI.Multitrack.t

  let string_of_kind = function `Midi -> "midi"
  let string_of_param (`Channels c) = Printf.sprintf "midi(channels=%d)" c
  let bytes _ = failwith "Not implemented!"

  let merge l l' =
    match (l, l') with
      | [], [] -> []
      | [x], [y] when x = y -> [x]
      | _ -> raise Invalid

  let blit src src_pos dst dst_pos len =
    Array.iter2
      (fun m m' ->
        let ( ! ) = midi_of_master in
        MIDI.blit m !src_pos m' !dst_pos !len)
      src dst

  let copy m = Array.map MIDI.copy m
  let params m = [`Channels (MIDI.Multitrack.channels m)]
  let kind = `Midi
  let default_params _ = [`Channels (Lazy.force Frame_settings.midi_channels)]

  let make l =
    let c =
      match l with
        | [] -> Lazy.force Frame_settings.midi_channels
        | [`Channels c] -> c
        | _ -> raise Invalid
    in
    MIDI.Multitrack.create c (midi_of_master !!size)

  let kind_of_string = function "midi" -> Some `Midi | _ -> None

  let param_of_string label value =
    match (label, value) with
      | "channels", c -> Some (`Channels (int_of_string c))
      | _ | (exception _) -> None
end

module Midi = MkContent (MidiSpecs)

let default_audio () =
  let channels = Lazy.force Frame_settings.audio_channels in
  if channels = 0 then None.format else Audio.format_of_channels channels

let default_video () =
  if Lazy.force Frame_settings.video_enabled then Video.lift_params []
  else None.format

let default_midi () =
  let channels = Lazy.force Frame_settings.midi_channels in
  if channels = 0 then None.format else Midi.lift_params [`Channels channels]

let is_internal f =
  None.is_kind f || Audio.is_kind f || Video.is_kind f || Midi.is_kind f
