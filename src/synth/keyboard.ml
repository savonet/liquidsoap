(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

let knotes =
  [|'a'; '?'; 'z'; '"'; 'e'; 'r'; '('; 't'; '-'; 'y'; '?';
    'u'; 'i'; '?'; 'o'; '?'; 'p'|]

let array_index a x =
  let ans = ref None in
    for i = 0 to Array.length knotes - 1 do
      if knotes.(i) = x then ans := Some i
    done;
    match !ans with
      | Some i -> i
      | None -> raise Not_found

let note_of_char c =
  array_index knotes c + 72

class keyboard ~kind =
object (self)
  inherit Source.active_source ~name:"input.keyboard" kind

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  val mutable ev = []
  val ev_m = Mutex.create ()

  method private add_event (t:int) (e:Midi.event) =
    Mutex.lock ev_m;
    ev <- (t,e)::ev;
    Mutex.unlock ev_m

  method private get_events =
    Mutex.lock ev_m;
    let e = List.rev ev in
      ev <- [];
      Mutex.unlock ev_m;
      e

  (** This source is not really clean for production, due to its
    * highly blocking reading task: it is hard to get it to
    * stop and restart cleanly when the source is asked to do so.
    * So we start it once for all and never stop it,
    * unless the full app stops. *)
  val mutable reader = None

  method private sleep = ()

  method private output_get_ready =
    if reader = None then
      let task () =
        while not !Root.shutdown do
          let c =
            let c = String.create 1 in
              ignore (Unix.read Unix.stdin c 0 1);
              c.[0]
          in
            try
              Printf.printf "\nPlaying note %d.\n%!" (note_of_char c);
              self#add_event 0 (Midi.Note_on (note_of_char c, 0.8))
               with
              | Not_found -> ()
        done
      in
        reader <- Some (Tutils.create task () "Virtual keyboard")

  method output_reset = ()
  method is_active = true

  method private get_frame frame =
    assert (0 = MFrame.position frame);
    let m = MFrame.content_of_type ~channels:1 frame 0 in
    let t = self#get_events in
      for c = 0 to Array.length m - 1 do
        m.(c) := t
      done;
      MFrame.add_break frame (MFrame.size ())

end

let () =
  Lang.add_operator "input.keyboard"
    []
    ~kind:Lang.midi_one
    ~category:Lang.Input
    ~flags:[Lang.Hidden; Lang.Experimental]
    ~descr:"Play notes from the keyboard."
    (fun p kind -> ((new keyboard ~kind):>Source.source))
