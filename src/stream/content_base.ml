(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

type 'a chunk = { data : 'a; offset : int; size : int }
type ('a, 'b) chunks = { mutable params : 'a; mutable chunks : 'b chunk list }

module Contents = struct
  type format = ..
  type kind = ..
  type _type = int
  type content = ..
  type data = _type * content

  let _type = ref 0

  let register_type () =
    incr _type;
    !_type

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
  List.filter_map
    (fun (l, v) -> match v with Some v -> Some (l ^ "=" ^ v) | None -> None)
    l
  |> String.concat ","

exception Invalid
exception Incompatible_format of Contents.format * Contents.format

module type ContentSpecs = sig
  type kind
  type params
  type data

  val make : size:int -> params -> data
  val length : data -> int
  val copy : data -> data
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
  val get_data : Contents.data -> (params, data) chunks
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

let parse_param kind label value =
  try
    Queue.iter
      (fun fn ->
        match fn kind label value with
          | Some p -> raise (Parsed_format p)
          | None -> ())
      format_parsers;
    raise Invalid
  with Parsed_format p -> p

type data_handler = {
  fill : data -> int -> data -> int -> int -> unit;
  length : data -> int;
  sub : data -> int -> int -> data;
  is_empty : data -> bool;
  copy : data -> data;
  format : data -> format;
  append : data -> data -> data;
}

let dummy_handler =
  {
    fill = (fun _ _ _ _ _ -> assert false);
    length = (fun _ -> assert false);
    sub = (fun _ _ _ -> assert false);
    is_empty = (fun _ -> assert false);
    copy = (fun _ -> assert false);
    format = (fun _ -> assert false);
    append = (fun _ -> assert false);
  }

let data_handlers = Array.make 12 dummy_handler

let register_data_handler t h =
  if Array.length data_handlers - 1 < t then
    failwith "Please increase media content array size!";
  Array.unsafe_set data_handlers t h

let get_data_handler (t, _) = Array.unsafe_get data_handlers t
let make ~size src = (get_params_handler src).make size
let length v = (get_data_handler v).length v
let fill v = (get_data_handler v).fill v
let sub v = (get_data_handler v).sub v
let is_empty v = (get_data_handler v).is_empty v
let copy v = (get_data_handler v).copy v
let format v = (get_data_handler v).format v
let append v = (get_data_handler v).append v
let kind p = (get_params_handler p).kind ()
let default_format f = (get_kind_handler f).default_format ()
let string_of_format k = (get_params_handler k).string_of_format ()

let () =
  Printexc.register_printer (function
    | Incompatible_format (f, f') ->
        Some
          (Printf.sprintf
             "Content.Incompatible_format: formats %s and %s are incompatible!"
             (string_of_format f) (string_of_format f'))
    | _ -> None)

let merge p p' =
  try (get_params_handler p).merge p'
  with _ ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Incompatible_format (p, p')) bt

let duplicate p = (get_params_handler p).duplicate ()
let compatible p p' = (get_params_handler p).compatible p'
let string_of_kind f = (get_kind_handler f).string_of_kind ()

module MkContent (C : ContentSpecs) = struct
  type Contents.kind += Kind of C.kind
  type Contents.format += Format of C.params Unifier.t
  type Contents.content += Content of (C.params, C.data) chunks

  let _type = Contents.register_type ()
  let content = function _, Content d -> d | _ -> assert false
  let params { params } = params

  let sub data ofs len =
    let start = ofs in
    let stop = start + len in
    {
      data with
      chunks =
        List.rev
          (snd
             (List.fold_left
                (fun (pos, cur) { data; offset; size } ->
                  let cur =
                    (* This is essentially a segment overlap calculation. *)
                    let start = max 0 (start - pos) in
                    let stop = min size (stop - pos) in
                    if start < stop then (
                      let offset = offset + start in
                      let size = stop - start in
                      { data; offset; size } :: cur)
                    else cur
                  in
                  (pos + size, cur))
                (0, []) data.chunks));
    }

  let length { chunks } =
    List.fold_left (fun cur { size } -> cur + size) 0 chunks

  let is_empty d = length d = 0

  let copy_chunks =
    List.map (fun chunk -> { chunk with data = C.copy chunk.data })

  let copy data = { data with chunks = copy_chunks data.chunks }

  let append d d' =
    let d = content d in
    let d' = content d' in
    (_type, Content { d with chunks = d.chunks @ d'.chunks })

  let fill src src_pos dst dst_pos len =
    let src = content src in
    let dst = content dst in
    dst.params <- src.params;
    let dst_len = length dst in
    dst.chunks <-
      (sub dst 0 dst_pos).chunks @ (sub src src_pos len).chunks
      @ (sub dst (dst_pos + len) (dst_len - len - dst_pos)).chunks;
    assert (dst_len = length dst)

  let make ~size params =
    { params; chunks = [{ data = C.make ~size params; offset = 0; size }] }

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

  let format_of_string kind label value =
    match kind with
      | Kind _ ->
          Option.map
            (fun p -> Format (Unifier.make p))
            (C.parse_param label value)
      | _ -> None

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
              make =
                (fun size -> (_type, Content (make ~size (Unifier.deref p))));
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
    let data_handler =
      {
        fill;
        length = (fun d -> length (content d));
        sub = (fun d ofs len -> (_type, Content (sub (content d) ofs len)));
        is_empty = (fun d -> is_empty (content d));
        copy = (fun d -> (_type, Content (copy (content d))));
        format = (fun d -> Format (Unifier.make (params (content d))));
        append;
      }
    in
    register_data_handler _type data_handler

  let is_kind = function Kind _ -> true | _ -> false
  let lift_kind f = Kind f
  let get_kind = function Kind f -> f | _ -> raise Invalid
  let is_format = function Format _ -> true | _ -> false
  let lift_params p = Format (Unifier.make p)
  let get_params = function Format p -> Unifier.deref p | _ -> raise Invalid
  let is_data = function _, Content _ -> true | _ -> false

  let lift_data d =
    ( _type,
      Content
        {
          params = C.params d;
          chunks = [{ offset = 0; size = C.length d; data = d }];
        } )

  let get_data = function _, Content d -> d | _ -> raise Invalid

  include C
end
