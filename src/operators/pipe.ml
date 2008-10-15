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

class pipe command source =
object (self)
  inherit Source.operator [source]

  (* Stub values *)
  val mutable pin = stdin
  val mutable pout = stdout

  method private wake_up l =
    source#get_ready ((self:>Source.source)::l) ;
    let i,o = Unix.open_process command in
      pin  <- i ;
      pout <- o

  method private sleep =
    ignore (Unix.close_process (pin,pout)) ;
    source#leave (self:>Source.source)

  method stype = source#stype (* Let's assume the command won't fail... *)
  method remaining = source#remaining
  method abort_track = source#abort_track
  method is_ready = source#is_ready

  method get_frame ab =
    let p1 = Mixer.Buffer.position ab in
    let p2 = source#get ab ; Mixer.Buffer.position ab in
    let s = Mixer.Buffer.to_string ab in
      output pout s p1 (p2-p1) ;
      flush pout ;
      for i = p1 to p2-1 do
        s.[i] <- input_char pin
      done

end

let () =
  Lang.add_operator "pipe" ~category:Lang.SoundProcessing
    ~descr:("Filter data through an external process. The process should read"^
            " raw CD format on stdin and write the same on stdout. The "^
            "command should output one frame for every input frame, and flush"^
            " its output. Otherwise, liquidsoap will block.")
    [ "", Lang.string_t, None, None ;
      "", Lang.source_t, None, None ]
    (fun p ->
       let c = Lang.to_string (Lang.assoc "" 1 p) in
       let s = Lang.to_source (Lang.assoc "" 2 p) in
         ((new pipe c s):>Source.source))
