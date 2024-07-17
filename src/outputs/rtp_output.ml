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

(** Output as a RTP stream. *)

class output source ~ip ~port ~ttl autostart =
object (self)
  inherit Output.output ~kind:"rtp" source autostart

  method output_reset = self#output_stop ; self#output_start

  val mutable session = None
  method output_start =
    session <- Some (Rtp.new_session ~ttl Rtp.Send ip port)
  method output_stop =
    session <- None (* TODO close session or something ! *)

  method output_send w =
    match session with
      | Some session -> Rtp.send ~nosync:true session w
      | None -> assert false

end

let () =
  Lang.add_operator "output.rtp"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

      "ip",
      Lang.string_t, Some (Lang.string "224.0.1.20"),
      Some "Broadcast address" ;

      "port",
      Lang.int_t, Some (Lang.int 8888),
      Some "Broadcast port" ;

      "ttl",
      Lang.int_t, Some (Lang.int 0),
      Some "Time to live: how far should the packets go ?" ;

      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Broadcast raw stream (includes metadata) using RTP."
    (fun p ->
       let start = Lang.to_bool (List.assoc "start" p) in
       let port = Lang.to_int (List.assoc "port" p) in
       let ttl = Lang.to_int (List.assoc "ttl" p) in
       let ip = Lang.to_string (List.assoc "ip" p) in
       let source = List.assoc "" p in
         ((new output source ~ip ~port ~ttl start):>Source.source))
