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

type 'a chunk = { data : 'a; offset : int; length : int option }
type ('a, 'b) chunks = { params : 'a; mutable chunks : 'b chunk list }

module Contents = struct
  type format_content
  type format = string * format_content Unifier.t
  type kind_content
  type kind = string * kind_content
  type _type = int
  type content
  type data = _type * content

  let _type = ref 0

  let register_type () =
    incr _type;
    !_type
end

let merge_param ?(compare = fun x x' -> x = x') ~name = function
  | None, None -> None
  | None, Some p | Some p, None -> Some p
  | Some p, Some p' when compare p p' -> Some p
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

  val name : string
  val make : ?length:int -> params -> data
  val is_sparse : bool
  val length : data -> int
  val blit : data -> int -> data -> int -> int -> unit
  val copy : data -> data
  val checksum : data -> string
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
  sub : data -> int -> int -> data;
  truncate : data -> int -> data;
  is_sparse : bool;
  _length : data -> int;
  is_empty : data -> bool;
  copy : data -> data;
  checksum : data -> string;
  format : data -> format;
  append : data -> data -> data;
}

let dummy_handler =
  {
    sub = (fun _ _ _ -> raise Invalid);
    truncate = (fun _ _ -> raise Invalid);
    is_sparse = false;
    _length = (fun _ -> raise Invalid);
    is_empty = (fun _ -> raise Invalid);
    copy = (fun _ -> raise Invalid);
    checksum = (fun _ -> raise Invalid);
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
let sub d = (get_data_handler d).sub d
let is_sparse d = (get_data_handler d).is_sparse
let truncate d = (get_data_handler d).truncate d
let is_empty c = (get_data_handler c).is_empty c
let length c = (get_data_handler c)._length c
let append c c' = (get_data_handler c).append c c'
let copy c = (get_data_handler c).copy c
let checksum c = (get_data_handler c).checksum c
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
let content_names = ref []

module MkContentBase (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type params = C.params
     and type data = C.data = struct
  include C

  let () =
    if List.mem C.name !content_names then
      failwith "content name already registered!";
    content_names := C.name :: !content_names

  type chunked_data = (C.params, C.data) chunks

  let _type = Contents.register_type ()

  let of_content : Contents.data -> chunked_data = function
    | t, d when t = _type -> Obj.magic d
    | _ -> raise Invalid

  let to_content : chunked_data -> Contents.data = fun d -> (_type, Obj.magic d)
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
    let d = of_content d in
    let d' = of_content d' in
    to_content { d with chunks = d.chunks @ d'.chunks }

  let consolidate_chunks =
    let consolidate_chunk ~buf pos ({ data; offset } as chunk) =
      let length = chunk_length chunk in
      if length > 0 then C.blit data offset buf pos length;
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

  let deref : Contents.format_content Unifier.t -> params =
   fun p -> Obj.magic (Unifier.deref p)

  let to_format_content : params -> Contents.format_content = Obj.magic
  let to_kind_content : kind -> Contents.kind_content = Obj.magic
  let to_kind : Contents.kind_content -> kind = Obj.magic

  let merge p p' =
    let p' = match p' with n, p' when n = C.name -> p' | _ -> raise Invalid in
    let m = C.merge (deref p) (deref p') in
    Unifier.set p' (to_format_content m);
    Unifier.(p <-- p')

  let compatible p p' =
    match p' with
      | n, p' when n = C.name -> C.compatible (deref p) (deref p')
      | _ -> false

  let is_kind (n, _) = n = C.name
  let lift_kind k : Contents.kind = (C.name, to_kind_content k)

  let get_kind = function
    | n, k when n = C.name -> to_kind k
    | _ -> raise Invalid

  let is_format (n, _) = n = C.name

  let lift_params p : Contents.format =
    (C.name, Unifier.make (to_format_content p))

  let get_params = function
    | n, p when n = C.name -> deref p
    | _ -> raise Invalid

  let is_data = function t, _ -> t = _type
  let kind_of_string s = Option.map lift_kind (C.kind_of_string s)

  let format_of_string kind label value =
    match (kind : Contents.kind) with
      | n, _ when n = C.name ->
          Option.map lift_params (C.parse_param label value)
      | _ -> None

  let () =
    register_kind_handler (function
      | n, k when n = C.name ->
          let k = to_kind k in
          Some
            {
              default_format = (fun () -> lift_params (C.default_params k));
              string_of_kind = (fun () -> C.string_of_kind k);
            }
      | _ -> None);
    register_format_handler (function
      | n, p when n = C.name ->
          Some
            {
              kind = (fun () -> lift_kind C.kind);
              make = (fun length -> to_content (make ?length (deref p)));
              merge = (fun p' -> merge p p');
              duplicate = (fun () -> (C.name, Unifier.(make (deref p))));
              compatible = (fun p' -> compatible p p');
              string_of_format =
                (fun () ->
                  let kind = C.string_of_kind C.kind in
                  let params = C.string_of_params (deref p) in
                  match params with
                    | "" -> kind
                    | _ -> Printf.sprintf "%s(%s)" kind params);
            }
      | _ -> None);
    Queue.push kind_of_string kind_parsers;
    Queue.push format_of_string format_parsers;
    let data_handler =
      {
        sub = (fun d ofs len -> to_content (sub (of_content d) ofs len));
        truncate = (fun d len -> to_content (truncate (of_content d) len));
        is_empty = (fun d -> is_empty (of_content d));
        is_sparse = C.is_sparse;
        copy = (fun d -> to_content (copy (of_content d)));
        checksum =
          (fun d ->
            let d = of_content d in
            let checksums =
              List.map
                (fun { data; offset; length } ->
                  let length =
                    match length with
                      | Some len -> len
                      | None -> max 0 (C.length data - offset)
                  in
                  Printf.sprintf "%d:%d:%s" offset length (C.checksum data))
                d.chunks
            in
            Digest.string (String.concat "|" checksums) |> Digest.to_hex);
        format = (fun d -> lift_params (params (of_content d)));
        _length = (fun d -> length (of_content d));
        append;
      }
    in
    register_data_handler _type data_handler

  let lift_data ?(offset = 0) ?length d =
    to_content { params = C.params d; chunks = [{ offset; length; data = d }] }

  let get_data d =
    let d = of_content d in
    match (consolidate_chunks ~copy:false d).chunks with
      | [] -> C.make ~length:0 d.params
      | [{ data }] -> data
      | _ -> raise Invalid

  include C
end
