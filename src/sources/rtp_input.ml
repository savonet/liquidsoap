(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

class input address port =
object (self)
  inherit Source.active_source

  method output_reset =
    (* TODO close current session ! *)
    self#output_get_ready

  (* Let's pretend and do as if there could be no failure,
   * so that the whole instance of liquidsoap will quit on RTP input error. *)
  method stype = Source.Infallible
  method remaining = -1
  method abort_track = ()

  val mutable session = None

  method output_get_ready =
    session <- Some (Rtp.new_session Rtp.Recv address port)

  val hm = ref 0
  method get_frame ab =
    assert (Mixer.Buffer.position ab = 0) ;
    match session with
      | Some session ->
          begin try
            Rtp.recv ~nosync:true session ab ;
            if !hm>0 then
              self#log 4 (Dtools.Log.f "have_more finished after %d" !hm) ;
            hm := 0
          with
            | Rtp.Have_more ->
                incr hm ;
                if !hm mod 1000 = 0 then
                  self#log 4 (Dtools.Log.f "many have_more: %d" !hm)
          end ;
          Mixer.Buffer.add_break ab Mixer.Buffer.size
      | None -> assert false

  method output = if Mixer.Buffer.is_partial memo then self#get_frame memo
end

let _ =
  Lang.add_operator "input.rtp"
    [ "ip",
      Lang.string_t, Some (Lang.string "224.0.1.20"),
      Some "Broadcast address" ;

      "port",
      Lang.int_t, Some (Lang.int 8888),
      Some "Broadcast port" ]
    ~descr:"Input raw stream (including metadata) using RTP."
    (fun p ->
       let port = Lang.to_int (List.assoc "port" p) in
       let ip = Lang.to_string (List.assoc "ip" p) in
         ((new input ip port):>Source.source))
