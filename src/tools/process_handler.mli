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

(** A process. *)
type t

(** Type for continuation decision. *)
type continuation =
  [ `Continue
  | `Stop
    (** triggers an asynchronous process stop, closing the process' stdin
             and waiting for termination *)
  | `Kill  (** kill the process immediately *)
  | `Delay of float  (** wait for a given amount of seconds *)
  | `Reschedule of Tutils.priority
    (** Update the process' priority and continue processing. *) ]

(** A call back. *)
type 'a callback = 'a -> continuation

(** Function for reading data. *)
type pull = Bytes.t -> int -> int -> int

(** Function for writing data. *)
type push = Bytes.t -> int -> int -> int

type status = [ `Exception of exn | `Status of Unix.process_status ]

(** Trying to performed an operation on a stopped process. *)
exception Finished

(** Create a process handler. Decisions returned by the callbacks are applied
   synchronously, i.e. when returning [`Stop], the process' stdin is closed
   immediately after the callback has returned. The process is restarted when
   the function [on_stop] returns [true]. *)
val run :
  ?priority:Tutils.priority ->
  ?env:string array ->
  ?on_start:push callback ->
  ?on_stdin:push callback ->
  ?on_stdout:pull callback ->
  ?on_stderr:pull callback ->
  ?on_stop:(status -> bool) ->
  ?log:(string -> unit) ->
  string ->
  t

(** Change the process' asynchronous task priority. Useful when switching from
    blocking read to non-blocking read. *)
val set_priority : t -> Tutils.priority -> unit

(** Asynchronous stop. The process' stdin will be closed some time in the
   future. *)
val stop : t -> unit

(** Asynchronous kill. The process will be killed some time in the future. *)
val kill : t -> unit

(** Whether the process was stopped. *)
val stopped : t -> bool

(** Write bytes recursively. *)
val really_write : ?offset:int -> ?length:int -> bytes -> push -> unit

(** Synchronous (blocking) write on the process' stdin. Raises [Finished] if the
   process has been stopped/killed. *)
val on_stdin : t -> (push -> 'a) -> 'a

(** Synchronous (blocking) read on the process' stdout. Raises [Finished] if the
   process has been stopped/killed. *)
val on_stdout : t -> (pull -> 'a) -> 'a

(** Synchronous (blocking) read on the process' stdout. Raises [Finished] if the
   process has been stopped/killed. *)
val on_stderr : t -> (pull -> 'a) -> 'a
