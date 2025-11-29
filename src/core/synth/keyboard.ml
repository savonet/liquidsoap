(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm

let knotes =
  [|
    'a';
    '?';
    'z';
    '"';
    'e';
    'r';
    '(';
    't';
    '-';
    'y';
    '?';
    'u';
    'i';
    '?';
    'o';
    '?';
    'p';
  |]

let array_index x =
  let ans = ref None in
  for i = 0 to Array.length knotes - 1 do
    if knotes.(i) = x then ans := Some i
  done;
  match !ans with Some i -> i | None -> raise Not_found

let note_of_char c = array_index c + 72

class keyboard =
  object (self)
    inherit Source.active_source ~name:"input.keyboard" ()
    method effective_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    method remaining = -1
    method abort_track = ()
    method self_sync = (`Static, None)
    method output = if self#is_ready then ignore self#get_frame
    val mutable ev = MIDI.create (MFrame.size ())
    val ev_m = Mutex.create ()

    method private add_event (t : int) (e : MIDI.event) =
      Mutex.lock ev_m;
      MIDI.insert ev (t, e);
      Mutex.unlock ev_m

    method private get_events =
      Mutex.lock ev_m;
      let e = MIDI.copy ev in
      MIDI.clear_all ev;
      Mutex.unlock ev_m;
      e

    (* Unique ID for runs (a run is delimited by get_ready/sleep,
     * used to manage the asynchronous task. *)
    val mutable run_id = 0
    val lock = Mutex.create ()

    initializer
      self#on_wake_up (fun () ->
          let id = run_id in
          let rec task _ =
            if run_id <> id then []
            else (
              let c =
                let c = Bytes.create 1 in
                ignore (Unix.read Unix.stdin c 0 1);
                Bytes.get c 0
              in
              begin try
                self#log#important "Playing note %d." (note_of_char c);
                self#add_event 0 (MIDI.Note_on (note_of_char c, 0.8))
              with Not_found -> ()
              end;
              [
                {
                  Duppy.Task.handler = task;
                  priority = `Non_blocking;
                  events = [`Read Unix.stdin];
                };
              ])
          in
          Duppy.Task.add Tutils.scheduler
            {
              Duppy.Task.handler = task;
              priority = `Non_blocking;
              events = [`Read Unix.stdin];
            });

      self#on_sleep (Mutex_utils.mutexify lock (fun () -> run_id <- run_id + 1))

    method reset = ()

    method private generate_frame =
      let t = self#get_events in
      Frame.set_data
        (Frame.create ~length:(Lazy.force Frame.size) Frame.Fields.empty)
        Frame.Fields.midi Content.Midi.lift_data [| t |]
  end

let input_keyboard =
  let return_t =
    Lang.frame_t Lang.unit_t (Frame.Fields.make ~midi:(Format_type.midi_n 1) ())
  in
  Lang.add_operator ~base:Modules.input "keyboard" [] ~return_t ~category:`Input
    ~flags:[`Hidden; `Experimental] ~descr:"Play notes from the keyboard."
    (fun _ -> (new keyboard :> Source.source))
