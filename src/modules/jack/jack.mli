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

val client_open : string -> client_open_option list -> client
val client_close : client -> unit
val get_sample_rate : client -> int
val get_buffer_size : client -> int

type port_flag =
  [ `IsInput | `IsOutput | `IsPhysical | `CanMonitor | `IsTerminal ]

val default_audio_type : string

val port_register :
  ?port_type:string ->
  ?buffer_size:int ->
  client ->
  string ->
  port_flag list ->
  port

val set_process_callback : client -> (int -> unit) -> unit

module Ringbuffer : sig
  type t

  val create : int -> t
  val mlock : t -> unit
  val read : t -> bytes -> int -> int -> int
  val read_space : t -> int
  val write : t -> bytes -> int -> int -> int
  val write_space : t -> int

  (** Read from the ringbuffer into a float32 Bigarray. Offset and count are in
      samples (float32 units). Returns the number of samples read. *)
  val read_to_ba : t -> buffer -> int -> int -> int

  (** Read all available data from the ringbuffer into a freshly allocated
      float64 OCaml array, converting from float32. *)
  val read_alloc : t -> float array

  (** Read from the ringbuffer into a float64 OCaml array, converting from
      float32. Offset and count are in samples. Returns the number of samples
      read. *)
  val read_to_buffer : t -> float array -> int -> int -> int

  (** Write from a float32 Bigarray into the ringbuffer. Offset and count are in
      samples (float32 units). Returns the number of samples written. *)
  val write_from_ba : t -> buffer -> int -> int -> int

  (** Write from a float64 OCaml array into the ringbuffer, converting to
      float32. Offset and count are in samples. Returns the number of samples
      written. *)
  val write_from_buffer : t -> float array -> int -> int -> int

  (** Advance the read pointer by n bytes without reading the data. *)
  val read_advance : t -> int -> unit
end

val activate : client -> unit
val port_get_buffer : port -> int -> buffer
val port_unregister : client -> port -> unit
val port_connect : client -> string -> string -> unit

(** Lightweight one-shot thread synchronization. Backed by a POSIX semaphore on
    Unix/macOS (via GCD dispatch semaphores on macOS) and a mutex+condition
    variable on Windows. One thread calls [wait] to block; another calls
    [signal] to unblock it. *)
module Wait : sig
  type t

  val create : unit -> t
  val signal : t -> unit
  val wait : t -> unit
end
