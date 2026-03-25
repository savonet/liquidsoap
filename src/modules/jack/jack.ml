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

type client
type port
type buffer = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type client_open_option = [ `NoStartServer | `ServerName of string ]

let client_open_flag = function `NoStartServer -> 0x01 | `ServerName _ -> 0x04

external _client_open : string -> int -> string option -> client
  = "caml_jack_client_open"

let client_open name options =
  let flags =
    List.fold_left (fun acc o -> acc lor client_open_flag o) 0 options
  in
  let server =
    List.find_map (function `ServerName s -> Some s | _ -> None) options
  in
  _client_open name flags server

external client_close : client -> unit = "caml_jack_client_close"

external get_sample_rate : client -> int = "caml_jack_get_sample_rate"
[@@noalloc]

external get_buffer_size : client -> int = "caml_jack_get_buffer_size"
[@@noalloc]

type port_flag =
  [ `IsInput | `IsOutput | `IsPhysical | `CanMonitor | `IsTerminal ]

let default_audio_type = "32 bit float mono audio"

let flag_to_int = function
  | `IsInput -> 0x1
  | `IsOutput -> 0x2
  | `IsPhysical -> 0x4
  | `CanMonitor -> 0x8
  | `IsTerminal -> 0x10

external _port_register : client -> string -> string -> int -> int -> port
  = "caml_jack_port_register"

let port_register ?(port_type = default_audio_type) ?(buffer_size = 0) client
    name flags =
  let flags_int = List.fold_left (fun acc f -> acc lor flag_to_int f) 0 flags in
  _port_register client name port_type flags_int buffer_size

external set_process_callback : client -> (int -> unit) -> unit
  = "caml_jack_set_process_callback"

module Ringbuffer = struct
  type t

  external create : int -> t = "caml_jack_ringbuffer_create"
  external mlock : t -> unit = "caml_jack_ringbuffer_mlock"

  external read : t -> bytes -> int -> int -> int = "caml_jack_ringbuffer_read"
  [@@noalloc]

  external read_space : t -> int = "caml_jack_ringbuffer_read_space" [@@noalloc]

  external write : t -> bytes -> int -> int -> int
    = "caml_jack_ringbuffer_write"
  [@@noalloc]

  external write_space : t -> int = "caml_jack_ringbuffer_write_space"
  [@@noalloc]

  external read_to_ba : t -> buffer -> int -> int -> int
    = "caml_jack_ringbuffer_read_ba"
  [@@noalloc]

  external read_alloc : t -> float array = "caml_jack_ringbuffer_read_alloc"

  external read_to_buffer : t -> float array -> int -> int -> int
    = "caml_jack_ringbuffer_read_array"
  [@@noalloc]

  external write_from_ba : t -> buffer -> int -> int -> int
    = "caml_jack_ringbuffer_write_ba"
  [@@noalloc]

  external write_from_buffer : t -> float array -> int -> int -> int
    = "caml_jack_ringbuffer_write_array"
  [@@noalloc]

  external read_advance : t -> int -> unit = "caml_jack_ringbuffer_read_advance"
  [@@noalloc]
end

external activate : client -> unit = "caml_jack_activate"
external port_get_buffer : port -> int -> buffer = "caml_jack_port_get_buffer"

external port_unregister : client -> port -> unit = "caml_jack_port_unregister"
[@@noalloc]

external port_connect : client -> string -> string -> unit
  = "caml_jack_port_connect"
[@@noalloc]

module Wait = struct
  type posix_sem

  type t =
    | Posix of posix_sem
    | Cond of { m : Mutex.t; c : Condition.t; triggered : bool Atomic.t }

  external posix_sem_create : unit -> posix_sem = "caml_jack_sem_create"

  external posix_sem_signal : posix_sem -> unit = "caml_jack_sem_post"
  [@@noalloc]

  external posix_sem_wait : posix_sem -> unit = "caml_jack_sem_wait"

  let create () =
    if Sys.win32 then
      Cond
        {
          m = Mutex.create ();
          c = Condition.create ();
          triggered = Atomic.make false;
        }
    else Posix (posix_sem_create ())

  let signal = function
    | Posix s -> posix_sem_signal s
    | Cond { m; c; triggered } ->
        Mutex.lock m;
        Atomic.set triggered true;
        Condition.signal c;
        Mutex.unlock m

  let wait = function
    | Posix s -> posix_sem_wait s
    | Cond { m; c; triggered } ->
        Mutex.lock m;
        while not (Atomic.get triggered) do
          Condition.wait c m
        done;
        Atomic.set triggered false;
        Mutex.unlock m
end
