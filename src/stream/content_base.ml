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

type 'a chunk = { data : 'a; offset : int; size : int }
type ('a, 'b) chunks = { mutable params : 'a; mutable chunks : 'b chunk list }

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
  val length : data -> int
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
  val get_chunked_data : Contents.data -> (params, data) chunks
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
  blit : int -> data -> int -> int -> unit;
  fill : int -> data -> int -> int -> unit;
  length : unit -> int;
  sub : int -> int -> data;
  is_empty : unit -> bool;
  copy : unit -> data;
  format : unit -> format;
  clear : unit -> unit;
  append : data -> data;
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
let length src = (get_data_handler src).length ()
let blit src = (get_data_handler src).blit
let fill src = (get_data_handler src).fill
let sub d = (get_data_handler d).sub
let is_empty c = (get_data_handler c).is_empty ()
let copy c = (get_data_handler c).copy ()
let format c = (get_data_handler c).format ()
let clear c = (get_data_handler c).clear ()
let append d = (get_data_handler d).append
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
  with _ -> raise (Incompatible_format (p, p'))

let duplicate p = (get_params_handler p).duplicate ()
let compatible p p' = (get_params_handler p).compatible p'
let string_of_kind f = (get_kind_handler f).string_of_kind ()

module MkContent (C : ContentSpecs) = struct
  type Contents.kind += Kind of C.kind
  type Contents.format += Format of C.params Unifier.t
  type Contents.data += Data of (C.params, C.data) chunks

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

  let append d = function
    | Data d' -> Data { d with chunks = d.chunks @ d'.chunks }
    | _ -> assert false

  let fill src src_pos dst dst_pos len =
    let dst = match dst with Data dst -> dst | _ -> raise Invalid in
    dst.params <- src.params;
    let dst_len = length dst in
    dst.chunks <-
      (sub dst 0 dst_pos).chunks @ (sub src src_pos len).chunks
      @ (sub dst (dst_pos + len) (dst_len - len - dst_pos)).chunks;
    assert (dst_len = length dst)

  let consolidate_chunks d =
    match (length d, d.chunks) with
      | 0, _ ->
          d.chunks <- [];
          d
      | _, [{ offset = 0; size; data }] when size = C.length data -> d
      | size, _ ->
          let buf = C.make ~size d.params in
          ignore
            (List.fold_left
               (fun pos { data; offset; size } ->
                 C.blit data offset buf pos size;
                 pos + size)
               0 d.chunks);
          d.chunks <- [{ offset = 0; size; data = buf }];
          d

  let blit src src_pos dst dst_pos len =
    let dst = match dst with Data dst -> dst | _ -> raise Invalid in
    dst.params <- src.params;
    let dst_len = length dst in
    dst.chunks <-
      (sub dst 0 dst_pos).chunks
      @ (consolidate_chunks (sub src src_pos len)).chunks
      @ (sub dst (dst_pos + len) (dst_len - len - dst_pos)).chunks;
    assert (dst_len = length dst)

  let clear d = List.iter (fun { data } -> C.clear data) d.chunks

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
              make = (fun size -> Data (make ~size (Unifier.deref p)));
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
              fill = fill d;
              length = (fun () -> length d);
              sub = (fun ofs len -> Data (sub d ofs len));
              is_empty = (fun () -> is_empty d);
              copy = (fun () -> Data (copy d));
              format = (fun () -> Format (Unifier.make (params d)));
              clear = (fun () -> clear d);
              append = append d;
            }
      | _ -> None)

  let is_kind = function Kind _ -> true | _ -> false
  let lift_kind f = Kind f
  let get_kind = function Kind f -> f | _ -> raise Invalid
  let is_format = function Format _ -> true | _ -> false
  let lift_params p = Format (Unifier.make p)
  let get_params = function Format p -> Unifier.deref p | _ -> raise Invalid
  let is_data = function Data _ -> true | _ -> false

  let lift_data d =
    Data
      {
        params = C.params d;
        chunks = [{ offset = 0; size = C.length d; data = d }];
      }

  let get_chunked_data = function Data d -> d | _ -> raise Invalid

  let get_data d =
    let d = get_chunked_data d in
    match (consolidate_chunks d).chunks with
      | [] -> C.make ~size:0 d.params
      | [{ offset = 0; size; data }] ->
          assert (size = C.length data);
          data
      | _ -> assert false

  include C
end
