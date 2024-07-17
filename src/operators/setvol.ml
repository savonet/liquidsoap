(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class setvol (source:source) override_field coeff =
object (self)
  inherit operator [source] as super

  val mutable override = None

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      begin match override_field with
        | Some f ->
            List.iter
              (fun (p,m) ->
                 if p >= offset then try
                   let s = Hashtbl.find m f in
                   let k =
                     try
                       Scanf.sscanf s " %f dB" Sutils.lin_of_dB
                     with
                       | _ -> float_of_string s
                   in
                     self#log#f 3 "Overriding amplification: %f." k ;
                     override <- Some k
                 with _ -> ())
              (AFrame.get_all_metadata buf)
        | None -> ()
      end ;
      let k = match override with Some o -> o | None -> coeff () in
        if k <> 1. then
          Float_pcm.multiply (AFrame.get_float_pcm buf)
            offset ((AFrame.position buf)-offset) k ;
        if AFrame.is_partial buf && override <> None then begin
          self#log#f 3 "End of the current overriding." ;
          override <- None
        end

end

let () = 
  Lang.add_operator "amplify"
    [ "", Lang.float_getter_t 1,  None, Some "Multiplicative factor." ;
      "override", Lang.string_t, Some (Lang.string "liq_amplify"),
      Some "Specify the name of a metadata field that, when present \
            and well-formed, overrides the amplification factor for the \
            current track. Well-formed values are floats in decimal notation \
            (e.g. '0.7') which are taken as normal/linear multiplicative \
            factors; values can be passed in decibels with the suffix 'dB' \
            (e.g. '-8.2 dB', but the spaces do not matter)." ;
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Multiply the amplitude of the signal."
    (fun p ->
       let c = Lang.to_float_getter (Lang.assoc "" 1 p) in
       let s = Lang.to_source (Lang.assoc "" 2 p) in
       let o = Lang.to_string (Lang.assoc "override" 1 p) in
       let o = if o = "" then None else Some (String.lowercase o) in
         ((new setvol s o c):>source))
