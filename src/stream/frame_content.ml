(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

module Contents = struct
  type format = ..
  type kind = ..
  type data = ..

  type audio_params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
  }

  type video_params = { width : int Lazy.t option; height : int Lazy.t option }
  type midi_params = { channels : int }
end

let merge_param ~name = function
  | None, None -> None
  | None, Some p | Some p, None -> Some p
  | Some p, Some p' when p = p' -> Some p
  | _ -> failwith ("Incompatible " ^ name)

let print_optional l =
  String.concat ","
    (List.fold_left
       (fun cur (lbl, v) ->
         match v with None -> cur | Some v -> (lbl ^ "=" ^ v) :: cur)
       [] l)

exception Invalid
exception Incompatible_format of Contents.format * Contents.format

module type ContentSpecs = sig
  type kind
  type params
  type data

  val make : size:int -> params -> data
  val blit : data -> int -> data -> int -> int -> unit
  val copy : data -> data
  val clear : data -> unit
  val params : data -> params
  val merge : params -> params -> params
  val compatible : params -> params -> bool
  val string_of_params : params -> string
  val parse_param : string -> string -> params option
  val kind : kind
  val default_params : kind -> params
  val string_of_kind : kind -> string
  val kind_of_string : string -> kind option
end

module type Content = sig
  include ContentSpecs

  val is_data : Contents.data -> bool
  val lift_data : data -> Contents.data
  val get_data : Contents.data -> data
  val is_format : Contents.format -> bool
  val lift_params : params -> Contents.format
  val get_params : Contents.format -> params
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
  make : int -> data;
  string_of_format : unit -> string;
  merge : format -> unit;
  compatible : format -> bool;
  duplicate : unit -> format;
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

let parse_param label value =
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
  copy : unit -> data;
  format : unit -> format;
  clear : unit -> unit;
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

let make ~size k = (get_params_handler k).make size

let blit src src_ofs dst dst_ofs len =
  let src = get_data_handler src in
  src.blit src_ofs dst dst_ofs len

let copy c = (get_data_handler c).copy ()
let format c = (get_data_handler c).format ()
let clear c = (get_data_handler c).clear ()
let kind p = (get_params_handler p).kind ()
let default_format f = (get_kind_handler f).default_format ()
let string_of_format k = (get_params_handler k).string_of_format ()

let () =
  Printexc.register_printer (function
    | Incompatible_format (f, f') ->
        Some
          (Printf.sprintf
             "Frame_content.Incompatible_format: %s and %s are not compatible \
              formats!"
             (string_of_format f) (string_of_format f'))
    | _ -> None)

let merge p p' =
  try (get_params_handler p).merge p'
  with _ -> raise (Incompatible_format (p, p'))

let duplicate p = (get_params_handler p).duplicate ()
let compatible p p' = (get_params_handler p).compatible p'
let string_of_kind f = (get_kind_handler f).string_of_kind ()

module MkContent (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type params = C.params
     and type data = C.data = struct
  type Contents.kind += Kind of C.kind
  type Contents.format += Format of C.params Unifier.t
  type Contents.data += Data of C.data

  let blit src src_ofs dst dst_ofs len =
    let dst = match dst with Data dst -> dst | _ -> raise Invalid in
    C.blit src src_ofs dst dst_ofs len

  let merge p p' =
    let p' = match p' with Format p' -> p' | _ -> raise Invalid in
    let m = C.merge (Unifier.deref p) (Unifier.deref p') in
    Unifier.set p' m;
    Unifier.(p <-- p')

  let compatible p p' =
    match p' with
      | Format p' -> C.compatible (Unifier.deref p) (Unifier.deref p')
      | _ -> false

  let kind_of_string s = Option.map (fun p -> Kind p) (C.kind_of_string s)

  let format_of_string label value =
    Option.map (fun p -> Format (Unifier.make p)) (C.parse_param label value)

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
      | Format p ->
          Some
            {
              kind = (fun () -> Kind C.kind);
              make = (fun size -> Data (C.make ~size (Unifier.deref p)));
              merge = (fun p' -> merge p p');
              duplicate = (fun () -> Format Unifier.(make (deref p)));
              compatible = (fun p' -> compatible p p');
              string_of_format =
                (fun () ->
                  let kind = C.string_of_kind C.kind in
                  let params = C.string_of_params (Unifier.deref p) in
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
              format = (fun () -> Format (Unifier.make (C.params d)));
              clear = (fun () -> C.clear d);
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
  type params = unit
  type data = unit

  let make ~size:_ _ = ()
  let clear _ = ()
  let blit _ _ _ _ _ = ()
  let copy _ = ()
  let params _ = ()
  let merge _ _ = ()
  let compatible _ _ = true
  let string_of_params _ = ""
  let parse_param _ _ = None
  let kind = ()
  let default_params () = ()
  let string_of_kind () = "none"
  let kind_of_string = function "none" -> Some () | _ -> None
end

module None = struct
  include MkContent (NoneSpecs)

  let data = lift_data ()
  let format = lift_params ()
end

module AudioSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Pcm ]
  type params = Contents.audio_params
  type data = Audio.Mono.buffer array

  let string_of_kind = function `Pcm -> "pcm"

  let string_of_params { channel_layout } =
    match !!channel_layout with
      | `Mono -> "mono"
      | `Stereo -> "stereo"
      | `Five_point_one -> "dolby 5.1"

  let merge p p' =
    assert (!!(p.channel_layout) = !!(p'.channel_layout));
    p

  let compatible p p' = !!(p.channel_layout) = !!(p'.channel_layout)

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
    | 1 -> { channel_layout = lazy `Mono }
    | 2 -> { channel_layout = lazy `Stereo }
    | 6 -> { channel_layout = lazy `Five_point_one }
    | _ -> raise Invalid

  let channels_of_param = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let parse_param label value =
    match (label, value) with
      | "", "mono" -> Some { channel_layout = lazy `Mono }
      | "", "stereo" -> Some { channel_layout = lazy `Stereo }
      | "", "5.1" -> Some { channel_layout = lazy `Five_point_one }
      | _ -> None

  let params d = param_of_channels (Array.length d)
  let kind = `Pcm

  let default_params _ =
    param_of_channels (Lazy.force Frame_settings.audio_channels)

  let clear _ = ()

  let make ~size { channel_layout } =
    let channels =
      match !!channel_layout with
        | `Mono -> 1
        | `Stereo -> 2
        | `Five_point_one -> 6
    in
    Array.init channels (fun _ -> Audio.Mono.create (audio_of_master size))

  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None
end

module Audio = struct
  open Contents
  include MkContent (AudioSpecs)

  let kind = lift_kind `Pcm

  let format_of_channels = function
    | 1 -> lift_params { channel_layout = lazy `Mono }
    | 2 -> lift_params { channel_layout = lazy `Stereo }
    | 6 -> lift_params { channel_layout = lazy `Five_point_one }
    | _ -> raise Invalid

  let channels_of_format p =
    AudioSpecs.(channels_of_param (Lazy.force (get_params p).channel_layout))
end

module VideoSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Yuv420p ]
  type params = Contents.video_params
  type data = Video.t

  let string_of_kind = function `Yuv420p -> "yuv420p"

  let make ~size { width; height } =
    let width = !!(Option.value ~default:video_width width) in
    let height = !!(Option.value ~default:video_height height) in
    Video.make (video_of_master size) width height

  let clear _ = ()

  let string_of_params { width; height } =
    print_optional
      [
        ("width", Option.map (fun x -> string_of_int !!x) width);
        ("height", Option.map (fun x -> string_of_int !!x) height);
      ]

  let parse_param label value =
    match label with
      | "width" ->
          Some { width = Some (lazy (int_of_string value)); height = None }
      | "height" ->
          Some { width = None; height = Some (lazy (int_of_string value)) }
      | _ -> None

  let merge p p' =
    {
      width = merge_param ~name:"width" (p.width, p'.width);
      height = merge_param ~name:"height" (p.height, p'.height);
    }

  let compatible p p' =
    let compare = function
      | None, None -> true
      | Some _, None | None, Some _ -> false
      | Some x, Some y -> !!x = !!y
    in
    compare (p.width, p'.width) && compare (p.height, p'.height)

  let blit src src_pos dst dst_pos len =
    let ( ! ) = Frame_settings.video_of_master in
    Video.blit src !src_pos dst !dst_pos !len

  let copy = Video.copy

  let params data =
    if Array.length data = 0 then { width = None; height = None }
    else (
      let i = data.(0) in
      {
        width = Some (lazy (Video.Image.width i));
        height = Some (lazy (Video.Image.height i));
      } )

  let kind = `Yuv420p
  let default_params _ = { width = None; height = None }

  let kind_of_string = function
    | "yuv420p" | "video" -> Some `Yuv420p
    | _ -> None
end

module Video = struct
  include MkContent (VideoSpecs)

  let kind = lift_kind `Yuv420p
end

module MidiSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Midi ]
  type params = Contents.midi_params
  type data = MIDI.Multitrack.t

  let string_of_kind = function `Midi -> "midi"
  let string_of_params { channels } = Printf.sprintf "channels=%d" channels

  let merge p p' =
    assert (p.channels = p'.channels);
    p

  let compatible p p' = p.channels = p'.channels

  let blit src src_pos dst dst_pos len =
    Array.iter2
      (fun m m' ->
        let ( ! ) = midi_of_master in
        MIDI.blit m !src_pos m' !dst_pos !len)
      src dst

  let copy m = Array.map MIDI.copy m
  let params m = { channels = MIDI.Multitrack.channels m }
  let kind = `Midi
  let default_params _ = { channels = Lazy.force Frame_settings.midi_channels }
  let clear _ = ()

  let make ~size { channels } =
    MIDI.Multitrack.create channels (midi_of_master size)

  let kind_of_string = function "midi" -> Some `Midi | _ -> None

  let parse_param label value =
    match (label, value) with
      | "channels", c -> Some { channels = int_of_string c }
      | _ | (exception _) -> None
end

module Midi = struct
  include MkContent (MidiSpecs)

  let kind = lift_kind `Midi
end

let default_audio () =
  let channels = Lazy.force Frame_settings.audio_channels in
  if channels = 0 then None.format else Audio.format_of_channels channels

let default_video () =
  if Lazy.force Frame_settings.default_video_enabled then
    Video.lift_params
      {
        Contents.width = Some Frame_settings.video_width;
        height = Some Frame_settings.video_height;
      }
  else None.format

let default_midi () =
  let channels = Lazy.force Frame_settings.midi_channels in
  if channels = 0 then None.format else Midi.lift_params { Contents.channels }

let is_internal f =
  None.is_kind f || Audio.is_kind f || Video.is_kind f || Midi.is_kind f
