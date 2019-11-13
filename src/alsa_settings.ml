(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Alsa related settings *)

(** ALSA should be quiet *)
let () = Alsa.no_stderr_report ()

(** Error translator *)
let error_translator e =
  match e with
    | Alsa.Buffer_xrun
    | Alsa.Bad_state
    | Alsa.Suspended
    | Alsa.IO_error
    | Alsa.Device_busy
    | Alsa.Invalid_argument
    | Alsa.Device_removed
    | Alsa.Interrupted
    | Alsa.Unknown_error _ ->
        Some (Printf.sprintf "Alsa error: %s" (Alsa.string_of_error e))
    | _ ->
        None

let () = Printexc.register_printer error_translator

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "alsa") "ALSA configuration"

let conf_buffer_length =
  Dtools.Conf.int
    ~p:(conf#plug "buffer_length")
    ~d:1 "Buffer size, in frames"
    ~comments:["This is only used for buffered ALSA I/O, and affects latency."]

let periods =
  Dtools.Conf.int ~p:(conf#plug "periods") ~d:0 "Number of periods"
    ~comments:["Set to 0 to disable this setting and use ALSA's default."]

let alsa_buffer =
  Dtools.Conf.int ~p:(conf#plug "alsa_buffer") ~d:0 "Alsa internal buffer size"
    ~comments:
      [ "This setting is only used in buffered alsa I/O, and affects latency.";
        "Set to 0 to disable this setting and use ALSA's default." ]

(** A dedicated clock for all ALSA I/O operators, to make sure other
  * blocking I/O inteferes with them. In the future, we might even want
  * to have different clocks for different ALSA devices. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.clock "alsa")
