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

class sequence ?(merge=false) sources =
object (self)
  inherit operator sources as super

  val mutable sources = sources
  initializer assert (sources <> [])

  method stype = (List.hd (List.rev sources))#stype

  method wake_up activation =
    List.iter
      (fun s -> (s:>source)#get_ready ((self:>source)::activation))
      sources

  method sleep = List.iter (fun s -> (s:>source)#leave (self:>source)) sources

  method is_ready = List.exists (fun s -> s#is_ready) sources

  method remaining =
    if merge then
      let (+) a b = if a<0 || b<0 then -1 else a+b in
        List.fold_left (+) 0 (List.map (fun s -> s#remaining) sources)
    else
      (List.hd sources)#remaining

  method abort_track =
    if merge then begin
      match List.rev sources with
        | [] -> assert false
        | hd::tl ->
            sources <- [hd] ;
            List.iter (fun (s:source) -> s#leave (self:>source)) tl
    end ;
    (List.hd sources)#abort_track

  val mutable head_ready = false

  method get_frame buf =
    if head_ready then begin
      let hd = List.hd sources in
        hd#get buf ;
        if List.length sources > 1 && Frame.is_partial buf then begin
          hd#leave (self:>source) ;
          head_ready <- false ;
          sources <- List.tl sources ;
          if merge && self#is_ready then self#get_frame buf
        end
    end else begin
      match sources with
        | a::(_::_ as tl) ->
            if a#is_ready then
              head_ready <- true
            else begin
              a#leave (self:>source) ;
              sources <- tl
            end ;
            self#get_frame buf
        | [a] ->
            assert (a#is_ready) ; (* Our #is_ready ensures that. *)
            head_ready <- true ;
            self#get_frame buf
        | [] -> assert false
    end

end

let () = 
  Lang.add_operator "sequence"
    [ "merge", Lang.bool_t, Some (Lang.bool false), None ;
      "", Lang.list_t Lang.source_t, None, None ]
    ~category:Lang.TrackProcessing
    ~descr:"Play only one track of every successive source, except for the last one which is played as much as available."
    (fun p ->
       ((new sequence
           ~merge:(Lang.to_bool (List.assoc "merge" p))
           (Lang.to_source_list (List.assoc "" p))):>source))
