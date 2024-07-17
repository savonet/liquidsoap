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

class filter (source:source) freq q filter_type =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = Mixer.Buffer.position buf in
      source#get buf ;
      Mixer.Buffer.simple_filter buf offset 
	((Mixer.Buffer.position buf) - offset) freq q filter_type

end

let _ = 
  Lang.add_operator "filter"
    [ "freq", Lang.int_t, None, None ;
      "q", Lang.float_t, None, None ;
      "mode", Lang.string_t, None, Some "low|high|band|notch" ;
      "", Lang.source_t, None, None ]
    ~descr:"Perform several kinds of filtering on the signal"
    (fun p ->
       let f v = List.assoc v p in
       let freq, q, mode, src =
         Lang.to_int (f "freq"),
         Lang.to_float (f "q"),
         f "mode",
         Lang.to_source (f "") in
       let mode =
         match Lang.to_string mode with
           | "low" -> Mixer.Buffer.Low_pass
           | "high" -> Mixer.Buffer.High_pass
           | "band" -> Mixer.Buffer.Band_pass
           | "notch" -> Mixer.Buffer.Notch
           | _ -> raise (Lang.Invalid_value
                           (mode,
                            "valid values are low|high|band|notch"))
       in
         ((new filter src freq q mode):>source))
