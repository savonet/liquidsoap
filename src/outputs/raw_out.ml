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

open Source
open Unix

class raw port autostart source =
object (self)
  inherit Output.output ~name:"prout" ~kind:"cadum" source autostart 

  val mutable socket = None

  method output_start =
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let bind_addr =
      ADDR_INET(inet_addr_of_string "127.0.0.1", port)
    in
      Unix.connect s bind_addr ;
      socket <- Some s

  method output_send b =
    match socket with
      | None -> assert false
      | Some fd ->
          let b = Mixer.Buffer.to_string b in
          let n = String.length b in
            assert (n = Unix.write fd b 0 n)

  method output_stop = ()

  method output_reset = ()

end

let _ =
    Lang.add_operator "output.raw"
      ~descr:"Minimalistic icecast server accepting one listener."
      [ "", Lang.int_t, None, Some "Port" ;
        "autostart", Lang.bool_t, Some (Lang.bool true), None ;
        "", Lang.source_t, None, None ]
      (fun p ->
         let port = Lang.to_int (Lang.assoc "" 1 p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let source = Lang.assoc "" 2 p in
           ((new raw port autostart source):>source))
