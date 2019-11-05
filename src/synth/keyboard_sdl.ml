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

let knotes2 =
  [|'&'; 'a'; '\233'; 'z'; '"'; 'e'; 'r'; '('; 't';
    '-'; 'y'; '\232'; 'u'; 'i'; '\231'; 'o'; '\224'; 'p'|]
let knotes1 =
  [|'q'; 'w'; 's'; 'x'; 'd'; 'c'; 'v'; 'g'; 'b';
    'h'; 'n'; 'j'; ','; ';'; 'l'; ':'; 'm'; '!'|]

let array_index a x =
  let ans = ref None in
    for i = 0 to Array.length a - 1 do
      if a.(i) = x then ans := Some i
    done;
    match !ans with
      | Some i -> i
      | None -> raise Not_found

let note_of_char c =
  try
    array_index knotes2 c + 71
  with
    | Not_found ->
        array_index knotes1 c + 59

class keyboard ~kind velocity =
  let () = Sdl_utils.init [`EVENTTHREAD; `VIDEO] in
object (self)
  inherit Source.active_source ~name:"input.keyboard.sdl" kind

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_get_ready =
    Sdlevent.disable_events (Sdlevent.all_events_mask);
    Sdlevent.enable_events
      (Sdlevent.make_mask
         [Sdlevent.KEYDOWN_EVENT; Sdlevent.KEYUP_EVENT; Sdlevent.QUIT_EVENT]);
    ignore (Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [])

  method private sleep = Sdl.quit ()

  val mutable reader = None

  val mutable velocity = velocity

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = MFrame.position frame);
    let m = Frame.content_of_type frame 0 (Frame.type_of_kind kind) in
    let m = m.Frame.midi in
    let t =
      let ans = MIDI.create (MFrame.size ()) in
        Sdlevent.pump ();
        while Sdlevent.has_event () do
          try
            match Sdlevent.poll () with
              | Some (Sdlevent.KEYDOWN k) ->
                  let c = Sdlkey.char_of_key k.Sdlevent.keysym in
                    if c = '+' || c = '*' then
                      velocity <- min 1. (velocity +. 0.1)
                    else if c = '-' || c = '/' then
                      velocity <- max 0. (velocity -. 0.1)
                    else
                      let n = note_of_char c in
                        (* Printf.printf "Playing note %d.\n%!" n; *)
                        MIDI.insert ans (0,MIDI.Note_on (n, velocity))
              | Some (Sdlevent.KEYUP k) ->
                  let c = Sdlkey.char_of_key k.Sdlevent.keysym in
                  let n = note_of_char c in
                    (* Printf.printf "Stopping note %d.\n%!" n; *)
                    MIDI.insert ans (0,MIDI.Note_off (n, velocity))
              | _ -> ()
          with
            | Not_found
            | Invalid_argument _ -> ()
        done;
        ans
    in
      for c = 0 to Array.length m - 1 do
        MIDI.clear_all m.(c);
        MIDI.merge m.(c) t
      done;
      MFrame.add_break frame (MFrame.size ())
end

let () =
  Lang.add_operator "input.keyboard.sdl"
    [
      "velocity", Lang.float_t, Some (Lang.float 0.8),
      Some "Velocity of notes."
    ]
    ~kind:(Lang.Constrained
             { Frame. audio = Lang.Any_fixed 0 ;
                      video = Lang.Fixed 0 ;
                      midi = Lang.Any_fixed 1 })
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Play notes from the keyboard."
    (fun p kind ->
       let f v = List.assoc v p in
       let velocity = Lang.to_float (f "velocity") in
         ((new keyboard ~kind velocity):>Source.source)
    )
