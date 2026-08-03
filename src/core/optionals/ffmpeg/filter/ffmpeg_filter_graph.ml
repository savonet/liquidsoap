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

(** The liquidsoap end of a filter graph.

    FFmpeg filters push from their inputs while liquidsoap pulls from its
    outputs, so the graph's inputs have to be driven on demand. That is what
    this source does: every output of the graph is a field of its frame, so a
    single pull loop feeds the whole graph, and the outputs advance together
    over one buffer, one set of track marks and one readiness test. *)

let conf_max_buffer =
  Dtools.Conf.float
    ~p:(Ffmpeg_utils.conf_ffmpeg#plug "filter_max_buffer")
    ~d:10.
    "Maximum amount of data (in seconds) a filter graph may hold for one of \
     its outputs before we consider that its outputs drain at diverging rates \
     and raise an error."

(** What the graph source needs from a sink, without knowing whether it carries
    audio or video. *)
type sink = {
  field : Frame.field;
  (* [false] until the graph is launched and this sink is connected. *)
  connected : unit -> bool;
  (* Move whatever the sink has ready into the generator. *)
  drain : generator:Generator.t -> unit;
  (* [true] once the graph has told this sink nothing more is coming. *)
  eof : unit -> bool;
}

class source ~name ~pull ~is_ready ~self_sync () =
  object (self)
    inherit Source.source ~name ()
    val mutable sinks = []
    method add_sink sink = sinks <- sinks @ [sink]
    method fallible = true
    method effective_source = (self :> Source.source)
    method self_sync : Clock.self_sync = self_sync (self :> Source.source)
    method remaining = Generator.remaining self#buffer

    method abort_track =
      Generator.clear self#buffer;
      Generator.add_track_mark self#buffer

    method! seek _ = 0

    (* Readiness must not tick anything: this is called from
       [before_streaming_cycle], where a clock tick is not ours to make. *)
    method private can_generate_frame =
      Lazy.force Frame.size <= Generator.length self#buffer || is_ready ()

    (* One output can legitimately run ahead of another while a filter fills
       its lookahead, so we wait on the slowest. If they never converge the
       fast one grows without bound, which is what this catches. *)
    method private check_buffer =
      let max_buffer = conf_max_buffer#get in
      if
        Frame.main_of_seconds max_buffer < Generator.buffered_length self#buffer
      then
        Runtime_error.raise
          ~pos:(match self#pos with Some p -> [p] | None -> [])
          ~message:
            (Printf.sprintf
               "Filter graph %s has buffered more than %.02fs of data for one \
                of its outputs. This happens when the graph's outputs are \
                consumed at diverging rates, which is not supported. Raise \
                `settings.ffmpeg.filter_max_buffer` if the graph legitimately \
                produces that much on a single pass."
               self#id max_buffer)
          "ffmpeg.filter"

    (* Ticking the graph's inputs is what makes its outputs produce, so alternate
       between draining the sinks and asking the inputs for more, until we have a
       frame or the inputs run out. *)
    method private fill_buffer =
      let size = Lazy.force Frame.size in
      let rec loop () =
        List.iter (fun sink -> sink.drain ~generator:self#buffer) sinks;
        self#check_buffer;
        (* Stop once every sink is done: no amount of pulling brings back a
           graph that has reached end of file. *)
        if
          Generator.length self#buffer < size
          && (not (List.for_all (fun sink -> sink.eof ()) sinks))
          && is_ready ()
        then (
          pull ();
          loop ())
      in
      (* Until the graph is launched there are no sinks to read from: ticking
         the inputs is what gets it there. *)
      let rec wait_for_launch () =
        if not (List.for_all (fun sink -> sink.connected ()) sinks) then
          if is_ready () then (
            pull ();
            (* [is_ready] launches a graph that has no input to wait for, so a
               second pass here means the launch is not going to happen. *)
            if List.for_all (fun sink -> sink.connected ()) sinks then ()
            else raise Exit)
          else raise Exit
      in
      try
        wait_for_launch ();
        loop ()
      with Exit -> ()

    method private generate_frame =
      let size = Lazy.force Frame.size in
      if Generator.length self#buffer < size then self#fill_buffer;
      Generator.slice self#buffer size
  end
