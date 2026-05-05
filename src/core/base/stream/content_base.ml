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

type 'a chunk = { data : 'a; offset : int; length : int }

type ('a, 'b) chunks = {
  params : 'a;
  mutable chunks : 'b chunk list;
  mutable total_length : int;
}

module Contents = struct
  type format_content
  type kind_content
  type 'a entry = { id : int; name : string; content : 'a }
  type format = format_content Unifier.t entry
  type kind = kind_content entry
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
  val content_lang_typ : Liquidsoap_lang.Type.t
  val params_to_value : params -> Liquidsoap_lang.Value.t
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

let dummy_kind_handler_fn (_ : Contents.kind_content) : kind_handler =
  raise Invalid

let kind_handler_fns : (Contents.kind_content -> kind_handler) array =
  Array.make 16 dummy_kind_handler_fn

let[@inline] get_kind_handler { Contents.id; content } =
  (Array.unsafe_get kind_handler_fns id) content

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

let dummy_format_handler_fn (_ : Contents.format_content Unifier.t) :
    format_handler =
  raise Invalid

let format_handler_fns :
    (Contents.format_content Unifier.t -> format_handler) array =
  Array.make 16 dummy_format_handler_fn

let[@inline] get_format_handler { Contents.id; content } =
  (Array.unsafe_get format_handler_fns id) content

let format_param_parsers : (string -> string -> Contents.format option) array =
  Array.make 16 (fun _ _ -> None)

let parse_param { Contents.id } label value =
  match (Array.unsafe_get format_param_parsers id) label value with
    | Some p -> p
    | None -> raise Invalid

type data_handler = {
  sub : data -> int -> int -> data;
  truncate : data -> int -> data;
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
    _length = (fun _ -> raise Invalid);
    is_empty = (fun _ -> raise Invalid);
    copy = (fun _ -> raise Invalid);
    checksum = (fun _ -> raise Invalid);
    format = (fun _ -> raise Invalid);
    append = (fun _ _ -> raise Invalid);
  }

let data_handlers = Array.make 16 dummy_handler

let register_data_handler t h =
  if Array.length data_handlers - 1 < t then
    failwith "Please increase media content array length!";
  Array.unsafe_set data_handlers t h

let[@inline] get_data_handler (t, _) = Array.unsafe_get data_handlers t
let make ?length k = (get_format_handler k).make length
let sub d = (get_data_handler d).sub d
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

type content_lang_spec = {
  format_name : string;
  method_name : string;
  content_typ : Liquidsoap_lang.Type.t;
  format_to_value : Contents.format -> Liquidsoap_lang.Value.t;
}

let content_lang_specs : content_lang_spec list ref = ref []

let register_content_lang format_name lang_name content_typ format_to_value =
  let method_name =
    String.map (fun c -> if c = '.' then '_' else c) lang_name
  in
  content_lang_specs :=
    { format_name; method_name; content_typ; format_to_value }
    :: !content_lang_specs

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

  let[@inline] of_content : Contents.data -> chunked_data = function
    | t, d when t = _type -> Obj.magic d
    | _ -> raise Invalid

  let[@inline] to_content : chunked_data -> Contents.data =
   fun d -> (_type, Obj.magic d)

  let[@inline] params { params } = params
  let[@inline] chunk_length { length; _ } = length
  let[@inline] length { total_length; _ } = total_length
  let[@inline] is_empty { total_length; _ } = total_length = 0

  (* Not tail-recursive, but chunk lists are always tiny in practice. *)
  let rec sub_chunks ofs stop pos = function
    | [] -> []
    | ({ data; offset } as chunk) :: rest ->
        let length = chunk_length chunk in
        let start = Int.max 0 (ofs - pos) in
        let chunk_stop = Int.min length (stop - pos) in
        let tail = sub_chunks ofs stop (pos + length) rest in
        if start < chunk_stop then
          { data; offset = offset + start; length = chunk_stop - start } :: tail
        else tail

  let sub data ofs len =
    let stop = ofs + len in
    if data.total_length < ofs || data.total_length < stop then
      raise (Invalid_argument "Content.sub");
    match data.chunks with
      | [({ offset; _ } as chunk)] ->
          let chunk_len = chunk_length chunk in
          let new_stop = Int.min chunk_len stop in
          {
            data with
            chunks =
              (if ofs < new_stop then
                 [{ chunk with offset = offset + ofs; length = new_stop - ofs }]
               else []);
            total_length = len;
          }
      | [chunk1; chunk2] ->
          (* Common case: Frame.append produces a 2-chunk frame, then Frame.slice
             calls sub on it. Avoid fold_left and tuple allocation per chunk. *)
          let len1 = chunk_length chunk1 in
          let chunks =
            if stop <= len1 then
              if ofs < stop then
                [
                  {
                    chunk1 with
                    offset = chunk1.offset + ofs;
                    length = stop - ofs;
                  };
                ]
              else []
            else if ofs >= len1 then (
              let local_ofs = ofs - len1 in
              let local_stop = stop - len1 in
              if local_ofs < local_stop then
                [
                  {
                    chunk2 with
                    offset = chunk2.offset + local_ofs;
                    length = local_stop - local_ofs;
                  };
                ]
              else [])
            else (
              let c2_len = stop - len1 in
              { chunk1 with offset = chunk1.offset + ofs; length = len1 - ofs }
              :: (if c2_len > 0 then [{ chunk2 with length = c2_len }] else []))
          in
          { data with chunks; total_length = len }
      | _ ->
          {
            data with
            chunks = sub_chunks ofs stop 0 data.chunks;
            total_length = len;
          }

  let rec truncate_chunks len = function
    | chunks when len = 0 -> chunks
    | chunk :: chunks ->
        let chunk_len = chunk_length chunk in
        if chunk_len <= len then truncate_chunks (len - chunk_len) chunks
        else (
          let { data; offset; length } = chunk in
          { data; offset = offset + len; length = length - len } :: chunks)
    | [] -> raise Invalid

  let truncate data len =
    assert (len <= data.total_length);
    {
      data with
      chunks = truncate_chunks len data.chunks;
      total_length = data.total_length - len;
    }

  let append d d' =
    let d = of_content d in
    let d' = of_content d' in
    let total_length =
      if d.total_length = max_int || d'.total_length = max_int then max_int
      else d.total_length + d'.total_length
    in
    let chunks =
      match (d.chunks, d'.chunks) with
        | chunks, [] -> chunks
        | [], chunks -> chunks
        | [c], [c'] -> [c; c']
        | _ -> d.chunks @ d'.chunks
    in
    to_content { d with chunks; total_length }

  let consolidate_chunks =
    let rec blit buf pos = function
      | [] -> ()
      | { data; offset; length } :: rest ->
          if length > 0 then C.blit data offset buf pos length;
          blit buf (pos + length) rest
    in
    fun ~copy d ->
      match (d.total_length, d.chunks) with
        | 0, _ ->
            d.chunks <- [];
            { d with chunks = [] }
        | _, [{ offset = 0; length; _ }] when length = max_int && not copy -> d
        | _, [{ offset = 0; length; data }]
          when length = C.length data && not copy ->
            d
        | length, _ ->
            let buf = C.make ~length d.params in
            blit buf 0 d.chunks;
            if copy then
              { d with chunks = [{ offset = 0; length; data = buf }] }
            else (
              d.chunks <- [{ offset = 0; length; data = buf }];
              d)

  let copy = consolidate_chunks ~copy:true

  let make ?length params =
    let data = C.make ?length params in
    let stored_length =
      match length with Some l -> l | None -> C.length data
    in
    let chunk = { data; offset = 0; length = stored_length } in
    { params; chunks = [chunk]; total_length = stored_length }

  let deref : Contents.format_content Unifier.t -> params =
   fun p -> Obj.magic (Unifier.deref p)

  let to_format_content : params -> Contents.format_content = Obj.magic
  let to_kind_content : kind -> Contents.kind_content = Obj.magic
  let to_kind : Contents.kind_content -> kind = Obj.magic

  let merge p p' =
    let p' =
      match p' with
        | { Contents.id; content } when id = _type -> content
        | _ -> raise Invalid
    in
    let m = C.merge (deref p) (deref p') in
    Unifier.set p' (to_format_content m);
    Unifier.(p <-- p')

  let compatible p p' =
    match p' with
      | { Contents.id; content } when id = _type ->
          C.compatible (deref p) (deref content)
      | _ -> false

  let[@inline] is_kind { Contents.id; _ } = id = _type

  let lift_kind k : Contents.kind =
    { Contents.id = _type; name = C.name; content = to_kind_content k }

  let get_kind { Contents.id; content } =
    if id = _type then to_kind content else raise Invalid

  let[@inline] is_format { Contents.id; _ } = id = _type

  let lift_params p : Contents.format =
    {
      Contents.id = _type;
      name = C.name;
      content = Unifier.make (to_format_content p);
    }

  let get_params { Contents.id; content } =
    if id = _type then deref content else raise Invalid

  let[@inline] is_data = function t, _ -> t = _type
  let kind_of_string s = Option.map lift_kind (C.kind_of_string s)

  let () =
    let kind_fn (k : Contents.kind_content) : kind_handler =
      let k = to_kind k in
      {
        default_format = (fun () -> lift_params (C.default_params k));
        string_of_kind = (fun () -> C.string_of_kind k);
      }
    in
    if Array.length kind_handler_fns <= _type then
      failwith "Please increase kind handler array length!";
    Array.unsafe_set kind_handler_fns _type kind_fn;
    let format_fn (p : Contents.format_content Unifier.t) : format_handler =
      {
        kind = (fun () -> lift_kind C.kind);
        make = (fun length -> to_content (make ?length (deref p)));
        merge = (fun p' -> merge p p');
        duplicate =
          (fun () ->
            {
              Contents.id = _type;
              name = C.name;
              content = Unifier.(make (deref p));
            });
        compatible = (fun p' -> compatible p p');
        string_of_format =
          (fun () ->
            let kind = C.string_of_kind C.kind in
            let params = C.string_of_params (deref p) in
            match params with
              | "" -> kind
              | _ -> Printf.sprintf "%s(%s)" kind params);
      }
    in
    if Array.length format_handler_fns <= _type then
      failwith "Please increase format handler array length!";
    Array.unsafe_set format_handler_fns _type format_fn;
    Array.unsafe_set format_param_parsers _type (fun label value ->
        Option.map lift_params (C.parse_param label value));
    Queue.push kind_of_string kind_parsers;
    let data_handler =
      {
        sub = (fun d ofs len -> to_content (sub (of_content d) ofs len));
        truncate = (fun d len -> to_content (truncate (of_content d) len));
        is_empty = (fun d -> is_empty (of_content d));
        copy = (fun d -> to_content (copy (of_content d)));
        checksum =
          (fun d ->
            let d = of_content d in
            let checksums =
              List.map
                (fun { data; offset; length } ->
                  let length =
                    if length = max_int then Int.max 0 (C.length data - offset)
                    else length
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
    let length =
      match length with
        | Some l -> l
        | None ->
            let l = C.length d in
            if l = max_int then max_int else l - offset
    in
    let chunk = { offset; length; data = d } in
    to_content
      {
        params = C.params d;
        chunks = [chunk];
        total_length = chunk_length chunk;
      }

  let get_data d =
    let d = of_content d in
    match (consolidate_chunks ~copy:false d).chunks with
      | [] -> C.make ~length:0 d.params
      | [{ data }] -> data
      | _ -> raise Invalid

  include C

  let () =
    register_content_lang C.name C.name C.content_lang_typ (fun fmt ->
        C.params_to_value (get_params fmt))
end
