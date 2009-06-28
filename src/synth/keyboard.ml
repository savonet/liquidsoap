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

class keyboard =
object (self)
  inherit Source.active_source

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  val mutable ev = []
  val ev_m = Mutex.create ()

  method add_event (e:Midi.event) =
    Mutex.lock ev_m;
    ev <- (0,e)::ev;
    Mutex.unlock ev_m

  method get_events =
    Mutex.lock ev_m;
    let e = List.rev ev in
      ev <- [];
      Mutex.unlock ev_m;
      e

  method output_get_ready =
    let _ =
      Tutils.create
        (fun () -> ()
        ) () "Virtual keyboard"
    in
      ()

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = MFrame.position frame);
    MFrame.add_break frame (MFrame.size frame)
end

let () =
  Lang.add_operator "input.keyboard"
    [
    ]
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Play notes from the keyboard."
    (fun p _ ->
       let e f v = f (List.assoc v p) in
         ((new keyboard):>Source.source)
    )
