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

class dyn ~kind f =
object (self)

  inherit Source.source ~name:"source.dynamic" kind

  val mutable source : Source.source option = None

  method stype = Source.Fallible
  method remaining = match source with Some s -> s#remaining | None -> -1
  method abort_track = match source with Some s -> s#abort_track | None -> ()

  val mutable activation = []

  method private wake_up ancestors =
    activation <- (self:>Source.source)::ancestors ;
    Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f ;
    self#select

  method private sleep =
    Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>Source.source)) f ;
    self#unregister_source

  method private unregister_source =
    match source with
      | Some s ->
          s#leave (self:>Source.source) ;
          source <- None
      | None -> ()

  method private select =
    let kind = Lang.kind_type_of_frame_kind kind in
    let l = Lang.apply ~t:(Lang.list_t (Lang.source_t kind)) f [] in
    let l = Lang.to_source_list l in
      match l with
        | [] -> ()
        | [s] ->
            self#unregister_source ;
            s#get_ready activation ;
            source <- Some s
        | _ -> assert false

  method is_ready =
    match source with Some s when s#is_ready -> true | _ -> false

  method private get_frame frame =
    begin match source with
      | Some s -> s#get frame
      | None -> Frame.add_break frame (Frame.position frame)
    end ;
    self#select

end

let () =
  let k = Lang.univ_t 1 in
    Lang.add_operator "source.dynamic"
      [ "", Lang.fun_t [] (Lang.list_t (Lang.source_t k)), None, None ]
      ~kind:(Lang.Unconstrained k)
      ~descr:"Dynamically change the underlying source."
      ~category:Lang.TrackProcessing (* TODO create better category *)
      (fun p kind -> new dyn kind (List.assoc "" p))
