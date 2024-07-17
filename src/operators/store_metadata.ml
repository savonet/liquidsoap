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

class store n s =
object (self)
  inherit Source.operator [s] as super

  method stype = s#stype
  method is_ready = s#is_ready
  method abort_track = s#abort_track
  method remaining = s#remaining

  val metadata_q = Queue.create ()

  val mutable ns = []
  method wake_up activation =
    if ns = [] then ns <- Server.register [self#id] "store_metadata" ;
    self#set_id (Server.to_string ns) ;
    super#wake_up activation ;
    Server.add ~ns "get"
      (fun _ ->
         let q = metadata_q in
           (fst (Queue.fold
                   (fun (s,i) m ->
                      let s = s^
                              (if s = "" then "--- " else "\n--- ")^
                              (string_of_int i)^" ---\n"^
                              (Request.string_of_metadata m) in
                        s,(i-1))
                   ("",(Queue.length q)) q)))

  method add_metadata m =
    Queue.add m metadata_q ;
    if Queue.length metadata_q > n then ignore (Queue.take metadata_q)

  method get_frame ab =
    s#get ab ;
    List.iter
      (fun (i,m) -> self#add_metadata (Hashtbl.copy m))
      (Frame.get_all_metadata ab)

end

let () =
  Lang.add_operator "store_metadata"
    [ "size", Lang.int_t, Some (Lang.int 10), Some "Size of the history" ;
      "", Lang.source_t, None, None ]
    ~category:Lang.TrackProcessing
    ~descr:("Keep track of the last N metadata packets in the stream, "^
            "and make the history available via a server command.")
    (fun p ->
       let s = Lang.to_source (List.assoc "" p) in
       let size = Lang.to_int (List.assoc "size" p) in
         ((new store size s):>Source.source))
