(*
   Copyright 2005-2020 Savonet team

   This file is part of Ocaml-alsa.

   Ocaml-alsa is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Ocaml-alsa is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Ocaml-alsa; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Interface with the alsa drivers.

    @author Samuel Mimram *)

(** Get the ALSA sound library version in ASCII format. *)
val get_version : unit -> string

(** A buffer underrun / overrun occurred. *)
exception Buffer_xrun

(** PCM is not in the right state. *)
exception Bad_state

(** A suspend event occurred (stream is suspended and waiting for an application
    * recovery). *)
exception Suspended

(** Input/output error. *)
exception IO_error

(** Device or resource was busy. *)
exception Device_busy

(** Function was called with an invalid argument. *)
exception Invalid_argument

(** This error can happen when device is physically * removed (for example some
    hotplug devices like USB * or PCMCIA, CardBus or ExpressCard can be removed
    on the fly). *)
exception Device_removed

exception Interrupted
exception Unknown_error of int

type direction = Dir_down | Dir_eq | Dir_up

(** Get an error message corresponding to an error. * Raise the given exception
    if it is not known. *)
val string_of_error : exn -> string

(** Do not report errors on stderr. *)
val no_stderr_report : unit -> unit

(** List all devices with name, description and IO. *)
val device_name_hints :
  ?card:int ->
  ?interface:string ->
  unit ->
  (string * string * [ `Input | `Output | `Both ]) list

module Pcm : sig
  (** Handle to a device. *)
  type handle

  (** Parameters of a device. *)
  type params

  (** Wanted stream. *)
  type stream =
    | Playback  (** Playback stream. *)
    | Capture  (** Capture stream. *)

  (** Modes for opening a stream. *)
  type mode =
    | Async  (** Asynchronous notification (not supported yet). *)
    | Non_blocking  (** Non blocking I/O. *)

  (** Open given device (use ["default"] for default one) with given streams and
      modes. *)
  val open_pcm : string -> stream list -> mode list -> handle

  (** Close device. *)
  val close : handle -> unit

  (** Prepare PCM for use. *)
  val prepare : handle -> unit

  (** Resume from suspend, no samples are lost. *)
  val resume : handle -> unit

  (** Recover the stream state from an error or suspend. This a high-level
      helper function building on other functions. This functions handles
      Interrupted, Buffer_xrun and Suspended exceptions trying to prepare given
      stream for next I/O. Raises the given exception when not recognized/used.
  *)
  val recover : ?verbose:bool -> handle -> exn -> unit

  (** Start the PCM. *)
  val start : handle -> unit

  (** Stop a PCM preserving pending frames. *)
  val drain : handle -> unit

  (** Stop a PCM dropping pending frames. *)
  val drop : handle -> unit

  (** [pause hnd pause] pauses (when [pause] is [true]) or resume (when [pause]
      is [false]) a PCM. *)
  val pause : handle -> bool -> unit

  val reset : handle -> unit

  (** Wait for a PCM to become ready. The second argument is the timeout in
      milliseconds (negative for infinite). Returns [false] if a timeout
      occurred. *)
  val wait : handle -> int -> bool

  (** [readi handle buf ofs len] reads [len] interleaved {i frames} in [buf]
      starting at offset [ofs] (in bytes). It returns the actual number of
      frames read. *)
  val readi : handle -> bytes -> int -> int -> int

  (** [writei handle buf ofs len] writes [len] interleaved {i frames} of [buf]
      starting at offset [ofs] (in bytes). *)
  val writei : handle -> bytes -> int -> int -> int

  (** Read non-interleaved frames. *)
  val readn : handle -> bytes array -> int -> int -> int

  (** Write non-interleaved frames. *)
  val writen : handle -> bytes array -> int -> int -> int

  val readn_float : handle -> float array array -> int -> int -> int
  val writen_float : handle -> float array array -> int -> int -> int

  (** Write in interleaved way non-interleaved frames. *)
  val writei_floatn : handle -> float array array -> int -> int -> int

  val readn_float_ba :
    handle ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int

  val writen_float_ba :
    handle ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int

  val writei_float_ba :
    handle ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int

  val readn_float64 : handle -> float array array -> int -> int -> int
  val writen_float64 : handle -> float array array -> int -> int -> int

  (** Get the delay (in frames). *)
  val get_delay : handle -> int

  (** State. *)
  type state =
    | St_open  (** open *)
    | St_setup  (** setup installed *)
    | St_prepared  (** ready to start *)
    | St_running  (** running *)
    | St_xrun  (** stopped: underrun (playback) or overrun (capture) detected *)
    | St_draining  (** draining: running (playback) or stopped (capture) *)
    | St_paused  (** paused *)
    | St_suspended  (** hardware is suspended *)
    | St_disconnected  (** hardware is disconnected *)

  (** Get the current state. *)
  val get_state : handle -> state

  val get_params : handle -> params
  val set_params : handle -> params -> unit

  (** Access mode. *)
  type access = Access_rw_interleaved | Access_rw_noninterleaved

  (** Set the access mode. *)
  val set_access : handle -> params -> access -> unit

  (** Format of audio data. *)
  type fmt =
    | Format_s16_le  (** 16 bits, little endian *)
    | Format_s24_3le
    | Format_float  (** float 32 bit CPU endian *)
    | Format_float64  (** float 64 bit CPU endian *)

  (** Set the format of audio data. *)
  val set_format : handle -> params -> fmt -> unit

  (** [set_rate_near handle params rate dir] sets the sampling rate (in Hz). *
      If the rate is not available, [dir] is used to determine the direction of
      * the nearest available sampling rate to use . The actual sampling rate
      used * is returned. *)
  val set_rate_near : handle -> params -> int -> direction -> int

  (** Set the number of channels. *)
  val set_channels : handle -> params -> int -> unit

  (** Set the number of periods. *)
  val set_periods : handle -> params -> int -> direction -> unit

  (** Get the number of periods. *)
  val get_periods_min : params -> int * direction

  val get_periods_max : params -> int * direction
  val get_period_size : params -> int

  (** Set the buffer size in {i frames}. *)
  val set_buffer_size : handle -> params -> int -> unit

  (** Set the buffer size near a value in {i frames}. *)
  val set_buffer_size_near : handle -> params -> int -> int

  val get_buffer_size : params -> int

  (* Get the buffer's size. *)
  val get_buffer_size_min : params -> int
  val get_buffer_size_max : params -> int

  (** Set blocking mode ([true] means non-blocking). *)
  val set_nonblock : handle -> bool -> unit

  (** Get the size of a frame in bytes. *)
  val get_frame_size : params -> int
end

module Sequencer : sig
  type t

  val create : string -> ?blocking:bool -> [ `Input | `Output | `Duplex ] -> t
  val set_client_name : t -> string -> unit

  type port_caps =
    | Port_cap_read
    | Port_cap_write
    | Port_cap_sync_read
    | Port_cap_sync_write
    | Port_cap_duplex
    | Port_cap_subs_read
    | Port_cap_subs_write
    | Port_cap_no_export

  type port_type =
    | Port_type_specific
    | Port_type_MIDI_generic
    | Port_type_MIDI_GM
    | Port_type_MIDI_GM2
    | Port_type_MIDI_GS
    | Port_type_MIDI_XG
    | Port_type_MIDI_MT32
    | Port_type_hardware
    | Port_type_software
    | Port_type_sythesizer
    | Port_type_port
    | Port_type_application

  val create_port : t -> string -> port_caps list -> port_type list -> int

  (** Real all possible input ports on given port. *)
  val subscribe_read_all : t -> int -> unit

  (** Write all possible output ports from given port. *)
  val subscribe_write_all : t -> int -> unit

  module Event : sig
    type note = {
      note_channel : int;
      note_note : int;
      note_velocity : int;
      note_off_velocity : int;
      note_duration : int;
    }

    type controller = {
      controller_channel : int;
      controller_param : int;
      controller_value : int;
    }

    type t =
      | System of int * int
      | Result of int * int
      | Note of note
      | Note_on of note
      | Note_off of note
      | Keypress of note
      | Controller of controller
      | Program_change of controller
      | Channel_pressure of controller
      | Pitch_bend of controller
      | Unhandled of int
  end

  type time = unit
  type event = { ev_event : Event.t; ev_time : time }

  val input_event : t -> event
  val output_event : t -> Event.t -> unit
end
