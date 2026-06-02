(*
  Copyright (C) 2003  Bardur Arantsson
  Copyright (C) 2004-2010 The Savonet Team

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

type device
type t = { device : device; mutable closed : bool }
type driver_kind_t = [ `LIVE | `FILE | `UNKNOWN ]
type byte_format_t = [ `LITTLE_ENDIAN | `BIG_ENDIAN | `NATIVE | `UNKNOWN ]

type driver_t = {
  id : int;
  kind : driver_kind_t;
  short_name : string;
  name : string;
  comment : string;
  author : string;
  priority : int;
  preferred_byte_format : byte_format_t;
  options : string list;
}

type internal_driver_t = int

exception Closed
exception Invalid_value

let () = Callback.register_exception "ocaml_ao_exn_invalid_value" Invalid_value

external _initialize : unit -> unit = "ocaml_ao_stubs_initialize"
external _shutdown : unit -> unit = "ocaml_ao_stubs_shutdown"

let () =
  _initialize ();
  at_exit _shutdown

external driver_kind : internal_driver_t -> driver_kind_t
  = "ocaml_ao_stubs_driver_kind"

external driver_name : internal_driver_t -> string
  = "ocaml_ao_stubs_driver_name"

external driver_short_name : internal_driver_t -> string
  = "ocaml_ao_stubs_driver_short_name"

external driver_comment : internal_driver_t -> string
  = "ocaml_ao_stubs_driver_comment"

external driver_author : internal_driver_t -> string
  = "ocaml_ao_stubs_driver_author"

external driver_priority : internal_driver_t -> int
  = "ocaml_ao_stubs_driver_priority"

external driver_preferred_byte_format : internal_driver_t -> byte_format_t
  = "ocaml_ao_stubs_driver_preferred_byte_format"

external driver_options : internal_driver_t -> string list
  = "ocaml_ao_stubs_driver_options"

let driver_of_internal_driver x =
  {
    id = x;
    kind = driver_kind x;
    short_name = driver_short_name x;
    name = driver_name x;
    comment = driver_comment x;
    author = driver_author x;
    priority = driver_priority x;
    preferred_byte_format = driver_preferred_byte_format x;
    options = driver_options x;
  }

external get_default_driver : unit -> internal_driver_t
  = "ocaml_ao_stubs_get_default_driver"

let get_default_driver () = driver_of_internal_driver (get_default_driver ())

external get_drivers : unit -> internal_driver_t list
  = "ocaml_ao_stubs_get_drivers"

let get_drivers () = List.map driver_of_internal_driver (get_drivers ())

external find_driver : string -> internal_driver_t
  = "ocaml_ao_stubs_find_driver"

let find_driver x = driver_of_internal_driver (find_driver x)
let drivers = get_drivers ()

external close : device -> unit = "ocaml_ao_stubs_close"

let gc_close x = if not x.closed then close x.device

let close x =
  if x.closed then raise Closed;
  close x.device;
  x.closed <- true

external open_live_aux :
  int ->
  int ->
  int ->
  string option ->
  byte_format_t ->
  (string * string) list ->
  internal_driver_t ->
  device
  = "ocaml_ao_stubs_open_live_aux_bytecode"
    "ocaml_ao_stubs_open_live_aux_native"

let open_live ?(bits = 16) ?(rate = 44100) ?(channels = 2) ?channels_matrix
    ?(byte_format = `LITTLE_ENDIAN) ?(options = []) ?driver () =
  let driver =
    match driver with None -> get_default_driver () | Some d -> d
  in
  let dev =
    open_live_aux bits rate channels channels_matrix byte_format options
      driver.id
  in
  let ret = { device = dev; closed = false } in
  Gc.finalise gc_close ret;
  ret

external open_file_aux :
  int ->
  int ->
  int ->
  string option ->
  byte_format_t ->
  (string * string) list ->
  internal_driver_t ->
  bool ->
  string ->
  device
  = "ocaml_ao_stubs_open_file_aux_bytecode"
    "ocaml_ao_stubs_open_file_aux_native"

let open_file ?(bits = 16) ?(rate = 44100) ?(channels = 2) ?channels_matrix
    ?(byte_format = `LITTLE_ENDIAN) ?(options = []) ?driver ?(overwrite = false)
    (filename : string) =
  let driver =
    match driver with None -> get_default_driver () | Some d -> d
  in
  let dev =
    open_file_aux bits rate channels channels_matrix byte_format options
      driver.id overwrite filename
  in
  let ret = { device = dev; closed = false } in
  Gc.finalise gc_close ret;
  ret

external play : device -> string -> unit = "ocaml_ao_stubs_play"

let play x s =
  if x.closed then raise Closed;
  play x.device s

let driver_kind d = d.kind
let driver_name d = d.name
let driver_short_name d = d.short_name
let driver_comment d = d.comment
let driver_author d = d.author
let driver_priority d = d.priority
let driver_preferred_byte_format d = d.preferred_byte_format
let driver_options d = d.options
