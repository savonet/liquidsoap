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

(** Generate a sine *)

open Source

class sine ~kind freq duration =
  let nb_samples = Frame.audio_of_seconds duration in
  let period = int_of_float (float (Lazy.force Frame.audio_rate) /. freq) in
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit source ~name:"sine" kind

  method stype = Infallible
  method is_ready = true

  (** The [remaining] variable is only used if [nb_samples>0]. *)
  val mutable remaining = nb_samples
  method remaining =
    if nb_samples>0 then Frame.master_of_audio remaining else -1

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  val mutable pos = 0

  method get_frame ab =
    if must_fail then begin
      AFrame.add_break ab (AFrame.position ab);
      remaining <- nb_samples;
      must_fail <- false
    end else
      let off = AFrame.position ab in
      let b = AFrame.content_of_type ~channels ab off in
      let size = AFrame.size () in
      let end_off =
        if nb_samples > 0 then
          min size (off + remaining)
        else
          size
      in
      let write i x =
        for c = 0 to Array.length b - 1 do
          b.(c).(i) <- x
        done
      in
        for i = off to end_off - 1 do
          write i (sin (float pos /. float period *. 2. *. 3.1416));
          pos <- pos + 1;
          if pos >= period then pos <- pos - period;
        done;
        AFrame.add_break ab end_off;
        if end_off < size then begin
          remaining <- nb_samples
        end else begin
          remaining <- remaining - end_off + off;
        end

end

let () =
  Lang.add_operator "sine"
    ~category:Lang.Input
    ~descr:"Generate a sine wave."
    ~kind:Lang.audio_any
    [ "duration", Lang.float_t, Some (Lang.float 0.), None ;
      "", Lang.float_t, Some (Lang.float 440.), Some "Frequency of the sine." ]
    (fun p kind ->
       (new sine ~kind
          (Lang.to_float (List.assoc "" p))
          (Lang.to_float (List.assoc "duration" p)) :> source))
