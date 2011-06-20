(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

class on_end ~kind ~delay f s =
object (self)
  inherit Source.operator kind [s]

  val mutable executed = false

  val mutable latest_metadata = None

  method stype = s#stype
  method is_ready = s#is_ready
  method remaining = s#remaining
  method abort_track = s#abort_track
  method seek n = s#seek n

  method private get_frame ab =
    s#get ab ;
    let compare x y = - (compare x y) in
    let l = 
      List.sort compare (Frame.get_all_metadata ab) 
    in
    if List.length l > 0 then
      latest_metadata <- Some (snd (List.hd l));
    let rem = Frame.seconds_of_master s#remaining in
    if rem <= delay && not executed then
    begin
      let m = 
        match latest_metadata with
          | Some m -> m
          | None -> Hashtbl.create 0
      in
      ignore(Lang.apply ~t:Lang.unit_t f ["",Lang.float rem;
                           "",Lang.metadata m]) ;
      executed <- true
    end ;
    if Frame.is_partial ab then
      executed <- false ;
end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "on_end"
    [ "delay", Lang.float_t,
      Some (Lang.float 5.),
      Some "Execute handler when remaining time is less or \
            equal to this value." ;
      "",
      Lang.fun_t
        [(false,"",Lang.float_t);
          false,"",Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)]
        Lang.unit_t,
      None,
      Some "Function to execute. First argument is the remaining time, \
            second is the latest metadata." ;
      "", Lang.source_t kind, None, None ]
    ~category:Lang.TrackProcessing
    ~descr:"Call a given handler when there is less than \
            a given amount of time remaining before then end of track."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let delay = Lang.to_float (List.assoc "delay" p) in 
       let f = Lang.assoc "" 1 p in
       let s = Lang.to_source (Lang.assoc "" 2 p) in
         new on_end ~kind ~delay f s)
