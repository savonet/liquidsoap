(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Asynchronous process handler. This module wraps around [Unix.open_process_full]
  * and [Unix.select] to manipulate external processes. Its API can be used in two
  * different ways:
  *  - Provide callbacks for reading and writting to the process' standard outputs.
  *    These callbacks are executed when [Unix.select] returns their respective
  *    file descriptors as available for writting/reading. Thus, read and write
  *    inside these callbacks should not be blocking although [Unix.select] 
  *    documentation stipulates that "large" writes may still be blocking.
  *  - Use direct, blocking, read/write on the process' standard output.
  *    in this case, reading and writting may be blocking, following the
  *    usual [Unix.write] and [Unix.read] semantics.
  * The API abstracts the file descriptors and provides writting/reading
  * callbacks instead. Please note that there is no way to detect when the
  * process has exited unless reaching a zero-length read in its standard 
  * output or error. Also, the default [on_stdout] and [on_stderr] callbacks
  * do not read from these. Therefore it is important to read all data coming
  * from at least one of these two outputs in order to make sure that the module
  * properly detects when the process has exited. *)

type t

(** Type for continuation decision.
  * [`Stop] triggers a asynchronous process stop,
  *         closing the process' stdin and waiting
  *         for termination.
  * [`Kill] kills the process immediately. *)
type continuation = [
  | `Continue
  | `Stop
  | `Kill
  | `Delay of float
]

type 'a callback = 'a -> continuation

type pull = Bytes.t -> int -> int -> int

type push = Bytes.t -> int -> int -> int

type status = [
  | `Exception of exn
  | `Status of Unix.process_status
]

exception Finished

(** Create a process handler. Decisions returned
  * by the callbacks are applied synchronously, i.e.
  * when returning [`Stop], the process' stdin is closed
  * immediately after the callback has returned. *)
val run : ?priority:Tutils.priority ->
          ?env:(string array) -> 
          ?on_start:(push callback) ->
          ?on_stdin:(push callback) ->
          ?on_stdout:(pull callback) ->
          ?on_stderr:(pull callback) ->
          ?on_stop:(status -> bool) ->
          ?log:(string->unit) ->
          string -> t

(** Asynchronous stop. The process' stdin will be closed
  * some time in the future. *)
val stop : t -> unit

(** Asynchronous kill. The process will be killed some
  * time in the future. *)
val kill : t -> unit

val stopped : t -> bool

val read : int -> pull -> string

val write : string -> push -> unit

(** Synchronous (blocking) write on the process' stdin. Raises [Finished]
  * if the process has been stopped/killed. *)
val on_stdin : t -> (push -> 'a) -> 'a

(** Synchronous (blocking) read on the process' stdout. Raises [Finished]
  * if the process has been stopped/killed. *)
val on_stdout : t -> (pull -> 'a) -> 'a

(** Synchronous (blocking) read on the process' stdout. Raises [Finished]
  * if the process has been stopped/killed. *)
val on_stderr : t -> (pull -> 'a) -> 'a
