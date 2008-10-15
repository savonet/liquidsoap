(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(** Avoids some source to play a new track before some delay elapses after the
  * end of the previous track played. *)

open Source

class delay (source:source) delay =
object (self)
  inherit operator [source] as super

  method stype = Fallible
  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable last = 0.
  (* failed means that we're not currently streaming *)
  val mutable failed = true

  method delay_ok = (Unix.time ()) -. last >= delay
  method is_ready = (not failed) || (self#delay_ok && source#is_ready)

  method get_frame buf =
    match failed,self#delay_ok with
      | false, _ ->
          source#get buf ;
          if Frame.is_partial buf then
            ( failed <- true ; last <- Unix.time () )
      | true, true ->
          failed <- false ;
          source#get buf
      | true, false -> ()

end

let () = 
  Lang.add_operator "delay"
    [ "", Lang.float_t, None,
      (Some ("The source won't be ready less than this amount of seconds "^
             "after any end of track")) ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.TrackProcessing
    ~descr:("Prevents the child from being ready again too fast after "^
            "a end of track")
    (fun p ->
       let f n = Lang.assoc "" n p in
       let d = Lang.to_float (f 1) in
       let s = Lang.to_source (f 2) in
         ((new delay s d):>source))
