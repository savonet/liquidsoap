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

(** A filter graph at run time (filters.md §8–§14): its graph inputs, sinks and
    graph source, the generations built from its description, and the pull loop
    that fills the graph source's buffer.

    The builtins of [ffmpeg.filter.create] call, while the graph value is in
    scope, {!add_instance}, {!link_input}, {!add_input}, {!add_output} and
    {!parse}, then {!close} when the script's function returns. Streaming is the
    graph source's: it fills from the generation on each frame, building one
    when every input knows its format. {!process_command} may run from any
    thread. *)

type t

(** The media-specific parts of graph inputs and buffer sources. *)
type ('m, 'fmt) media

type sink

val audio : ([ `Audio ], Avutil.Audio.frame_format) media
val video : ([ `Video ], Avutil.Video.frame_format) media
val create : unit -> t
val description : t -> Description.t
val in_scope : t -> bool

(** Attaches an instance in the probe config with its arguments.
    @raise Avutil.Error when FFmpeg rejects it. *)
val add_instance :
  t ->
  [ `Unattached ] Avfilter.filter ->
  Avfilter.args list ->
  Description.element

val link_input :
  t ->
  'm Description.pad ->
  Description.element ->
  Description.media ->
  int ->
  ([ `Attached ] Avfilter.filter ->
  ([ `Attached ], 'm, [ `Input ]) Avfilter.pad list) ->
  unit

(** A graph input reading the field of a source, named [id]. *)
val add_input :
  t ->
  ('m, _) media ->
  id:string ->
  pass_metadata:bool ->
  Frame.field * Source.source ->
  'm Description.pad

val audio_sink :
  t ->
  element:Description.element ->
  field:Frame.field ->
  pass_metadata:bool ->
  sink

val video_sink :
  t ->
  element:Description.element ->
  field:Frame.field ->
  pass_metadata:bool ->
  sink

(** A sink of the graph source, created on the first output, under the next
    field of its media. *)
val add_output :
  t ->
  ('m, _) media ->
  make_sink:
    (t ->
    element:Description.element ->
    field:Frame.field ->
    pass_metadata:bool ->
    sink) ->
  pass_metadata:bool ->
  'm Description.pad ->
  Frame.field * Source.source

(** @raise Failure
      for a buffer filter in the description, and the bindings' errors of the
      replay. *)
val parse :
  t ->
  description:string ->
  audio_inputs:(string * [ `Audio ] Description.pad) list ->
  video_inputs:(string * [ `Video ] Description.pad) list ->
  audio_outputs:string list ->
  video_outputs:string list ->
  (string * [ `Audio ] Description.pad) list
  * (string * [ `Video ] Description.pad) list

(** FFmpeg's answer, ["graph not started!"] with no generation, FFmpeg's error
    message when it refuses. Never raises. *)
val process_command :
  t -> Description.element -> fast:bool -> string -> string -> string

(** Checks completeness and sets up the clocks (§7.2 steps 3 to 5).
    @raise Failure naming an unlinked pad. *)
val close : t -> pos:Pos.t option -> unit
