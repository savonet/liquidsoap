(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

type 'a chunk = { data : 'a; offset : int; length : int option }
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

  val make : ?length:int -> params -> data
  val length : data -> int
  val blit : data -> int -> data -> int -> int -> unit
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
  val lift_data : ?offset:int -> ?length:int -> data -> Contents.data
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
  make : int option -> data;
  string_of_format : unit -> string;
  merge : format -> unit;
  compatible : format -> bool;
  duplicate : unit -> format;
}

let format_handlers = Queue.create ()
let register_format_handler fn = Queue.add fn format_handlers

exception Found_format of format_handler

let get_format_handler p =
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
  sub : data -> int -> int -> data;
  truncate : data -> int -> data;
  _length : data -> int;
  is_empty : data -> bool;
  copy : data -> data;
  format : data -> format;
  append : data -> data -> data;
}

let dummy_handler =
  {
    fill = (fun _ _ _ _ _ -> raise Invalid);
    sub = (fun _ _ _ -> raise Invalid);
    truncate = (fun _ _ -> raise Invalid);
    _length = (fun _ -> raise Invalid);
    is_empty = (fun _ -> raise Invalid);
    copy = (fun _ -> raise Invalid);
    format = (fun _ -> raise Invalid);
    append = (fun _ _ -> raise Invalid);
  }

let data_handlers = Array.make 12 dummy_handler

let register_data_handler t h =
  if Array.length data_handlers - 1 < t then
    failwith "Please increase media content array length!";
  Array.unsafe_set data_handlers t h

let get_data_handler (t, _) = Array.unsafe_get data_handlers t
let make ?length k = (get_format_handler k).make length
let fill src = (get_data_handler src).fill src
let sub d = (get_data_handler d).sub d
let truncate d = (get_data_handler d).truncate d
let is_empty c = (get_data_handler c).is_empty c
let length c = (get_data_handler c)._length c
let append c c' = (get_data_handler c).append c c'
let copy c = (get_data_handler c).copy c
let format c = (get_data_handler c).format c
let kind p = (get_format_handler p).kind ()
let default_format f = (get_kind_handler f).default_format ()
let string_of_format k = (get_format_handler k).string_of_format ()

let () =
  Printexc.register_printer (function
    | Incompatible_format (f, f') ->
        Some
          (Printf.sprintf
             "Content.Incompatible_format: formats %s and %s are incompatible!"
             (string_of_format f) (string_of_format f'))
    | _ -> None)

let merge p p' =
  let { merge } = get_format_handler p in
  try merge p' with _ -> raise (Incompatible_format (p, p'))

let duplicate p = (get_format_handler p).duplicate ()
let compatible p p' = (get_format_handler p).compatible p'
let string_of_kind k = (get_kind_handler k).string_of_kind ()

module MkContentBase (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type params = C.params
     and type data = C.data = struct
  type Contents.kind += Kind of C.kind
  type Contents.format += Format of C.params Unifier.t
  type Contents.content += Content of (C.params, C.data) chunks

  let _type = Contents.register_type ()
  let content = function _, Content d -> d | _ -> raise Invalid
  let params { params } = params

  let chunk_length { data; offset; length } =
    match length with
      | Some len -> len
      | None ->
          (* This is a hack ok. *)
          let len = C.length data in
          if len = max_int then max_int else len - offset

  let length { chunks } =
    List.fold_left
      (fun cur chunk ->
        let chunk_length = chunk_length chunk in
        if cur = max_int || chunk_length = max_int then max_int
        else cur + chunk_length)
      0 chunks

  let is_empty d = length d = 0

  let sub data ofs len =
    let start = ofs in
    let stop = start + len in
    let data_length = length data in
    if data_length < start || data_length < stop then
      raise (Invalid_argument "Content.sub");
    {
      data with
      chunks =
        List.rev
          (snd
             (List.fold_left
                (fun (pos, cur) ({ data; offset } as chunk) ->
                  let length = chunk_length chunk in
                  let cur =
                    (* This is essentially a segment overlap calculation. *)
                    let start = max 0 (start - pos) in
                    let stop = min length (stop - pos) in
                    if start < stop then (
                      let offset = offset + start in
                      let length = Some (stop - start) in
                      { data; offset; length } :: cur)
                    else cur
                  in
                  (pos + length, cur))
                (0, []) data.chunks));
    }

  let truncate data len =
    assert (len <= length data);
    let rec f len = function
      | chunks when len = 0 -> chunks
      | chunk :: chunks when chunk_length chunk < len ->
          f (len - chunk_length chunk) chunks
      | { data; offset; length } :: chunks ->
          {
            data;
            offset = offset + len;
            length = Option.map (fun l -> l - len) length;
          }
          :: chunks
      | [] -> raise Invalid
    in
    { data with chunks = f len data.chunks }

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

  let consolidate_chunks =
    let consolidate_chunk ~buf pos ({ data; offset } as chunk) =
      let length = chunk_length chunk in
      C.blit data offset buf pos length;
      pos + length
    in
    fun ~copy d ->
      match (length d, d.chunks) with
        | 0, _ ->
            d.chunks <- [];
            { d with chunks = [] }
        | _, [{ offset = 0; length = None }] when not copy -> d
        | _, [{ offset = 0; length = Some l; data }]
          when l = C.length data && not copy ->
            d
        | length, _ ->
            let buf = C.make ~length d.params in
            ignore (List.fold_left (consolidate_chunk ~buf) 0 d.chunks);
            if copy then
              {
                d with
                chunks = [{ offset = 0; length = Some length; data = buf }];
              }
            else (
              d.chunks <- [{ offset = 0; length = Some length; data = buf }];
              d)

  let copy = consolidate_chunks ~copy:true

  let make ?length params =
    { params; chunks = [{ data = C.make ?length params; offset = 0; length }] }

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
      | Kind k ->
          Some
            {
              default_format =
                (fun () -> Format (Unifier.make (C.default_params k)));
              string_of_kind = (fun () -> C.string_of_kind k);
            }
      | _ -> None);
    register_format_handler (function
      | Format p ->
          Some
            {
              kind = (fun () -> Kind C.kind);
              make =
                (fun length ->
                  (_type, Content (make ?length (Unifier.deref p))));
              merge = (fun p' -> merge p p');
              duplicate = (fun () -> Format Unifier.(make (deref p)));
              compatible = (fun p' -> compatible p p');
              string_of_format =
                (fun () ->
                  let kind = C.string_of_kind C.kind in
                  let params = C.string_of_params (Unifier.deref p) in
                  match params with
                    | "" -> kind
                    | _ -> Printf.sprintf "%s(%s)" kind params);
            }
      | _ -> None);
    Queue.push kind_of_string kind_parsers;
    Queue.push format_of_string format_parsers;
    let data_handler =
      {
        fill;
        sub = (fun d ofs len -> (_type, Content (sub (content d) ofs len)));
        truncate = (fun d len -> (_type, Content (truncate (content d) len)));
        is_empty = (fun d -> is_empty (content d));
        copy = (fun d -> (_type, Content (copy (content d))));
        format = (fun d -> Format (Unifier.make (params (content d))));
        _length = (fun d -> length (content d));
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

  let lift_data ?(offset = 0) ?length d =
    ( _type,
      Content { params = C.params d; chunks = [{ offset; length; data = d }] }
    )

  let get_chunked_data = function _, Content d -> d | _ -> raise Invalid

  let get_data d =
    let d = get_chunked_data d in
    match (consolidate_chunks ~copy:false d).chunks with
      | [] -> C.make ~length:0 d.params
      | [{ data }] -> data
      | _ -> raise Invalid

  include C
end
