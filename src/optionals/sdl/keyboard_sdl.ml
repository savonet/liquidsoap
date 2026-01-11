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

open Mm
open Tsdl

let knotes2 =
  [|
    '&';
    'a';
    '\233';
    'z';
    '"';
    'e';
    'r';
    '(';
    't';
    '-';
    'y';
    '\232';
    'u';
    'i';
    '\231';
    'o';
    '\224';
    'p';
  |]

let knotes1 =
  [|
    'q';
    'w';
    's';
    'x';
    'd';
    'c';
    'v';
    'g';
    'b';
    'h';
    'n';
    'j';
    ',';
    ';';
    'l';
    ':';
    'm';
    '!';
  |]

let array_index a x =
  let ans = ref None in
  for i = 0 to Array.length a - 1 do
    if a.(i) = x then ans := Some i
  done;
  match !ans with Some i -> i | None -> raise Not_found

let char_of_key k =
  let c = Sdl.get_key_name k in
  if c = "" then raise Not_found;
  c.[0]

let note_of_char c =
  try array_index knotes2 c + 71 with Not_found -> array_index knotes1 c + 59

class keyboard velocity =
  let () = Sdl_utils.init [Sdl.Init.events; Sdl.Init.video] in
  object (self)
    inherit Source.active_source ~name:"input.keyboard.sdl" ()
    method effective_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    method remaining = -1
    method abort_track = ()
    method self_sync = (`Static, None)
    method output = if self#is_ready then ignore self#get_frame
    val mutable window = None

    initializer
      self#on_wake_up (fun () ->
          window <-
            Some
              (Sdl_utils.check
                 (fun () ->
                   Sdl.create_window "Liquidsoap" ~w:640 ~h:480
                     Sdl.Window.windowed)
                 ()));

      (* TODO: could this be too radical? *)
      self#on_sleep Sdl.quit

    val mutable reader = None
    val mutable velocity = velocity
    method reset = ()

    method generate_frame =
      let t =
        let ans = MIDI.create (MFrame.size ()) in
        Sdl.pump_events ();
        while
          Sdl.has_event Sdl.Event.key_down || Sdl.has_event Sdl.Event.key_up
        do
          try
            let e = Sdl.Event.create () in
            if Sdl.poll_event (Some e) then (
              match Sdl.Event.(enum (get e typ)) with
                | `Key_down ->
                    let k = Sdl.Event.(get e keyboard_keycode) in
                    let c = char_of_key k in
                    if c = '+' || c = '*' then
                      velocity <- min 1. (velocity +. 0.1)
                    else if c = '-' || c = '/' then
                      velocity <- max 0. (velocity -. 0.1)
                    else (
                      let n = note_of_char c in
                      (* Printf.printf "Playing note %d.\n%!" n; *)
                      MIDI.insert ans (0, MIDI.Note_on (n, velocity)))
                | `Key_up ->
                    let k = Sdl.Event.(get e keyboard_keycode) in
                    let c = char_of_key k in
                    let n = note_of_char c in
                    (* Printf.printf "Stopping note %d.\n%!" n; *)
                    MIDI.insert ans (0, MIDI.Note_off (n, velocity))
                | _ -> ())
          with Not_found | Invalid_argument _ -> ()
        done;
        ans
      in
      Frame.set_data self#empty_frame Frame.Fields.midi Content.Midi.lift_data
        [| t |]
  end

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t (Frame.Fields.make ~midi:(Format_type.midi ()) ())
  in
  Lang.add_operator ~base:Keyboard.input_keyboard "sdl"
    [
      ( "velocity",
        Lang.float_t,
        Some (Lang.float 0.8),
        Some "Velocity of notes." );
    ]
    ~return_t ~category:`Input ~flags:[`Experimental]
    ~descr:"Play notes from the keyboard."
    (fun p ->
      let f v = List.assoc v p in
      let velocity = Lang.to_float (f "velocity") in
      (new keyboard velocity :> Source.source))
