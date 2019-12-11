(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

class dyn ~kind f =
  object (self)
    inherit Source.source ~name:"source.dynamic" kind

    method stype = Source.Fallible

    val mutable activation = []

    (* The dynamic stuff: #select calls the selection function and changes
     * the source when needed, and #unregister_source does what its name says.
     * Any sequence of calls to #select and #unregister_source is okay
     * but they should not overlap.
     * All that matters for cleanliness is that #unregister_source comes
     * last, which #sleep ensures. *)
    val source_lock = Mutex.create ()

    val mutable source : Source.source option = None

    method private unregister_source ~already_locked =
      let unregister () =
        match source with
          | Some s ->
              s#leave (self :> Source.source) ;
              source <- None
          | None ->
              ()
      in
      if already_locked then unregister ()
      else Tutils.mutexify source_lock unregister ()

    method private select =
      (* Avoid that a new source gets assigned to the default clock. *)
      Clock.collect_after
        (Tutils.mutexify source_lock (fun () ->
             let kind = Lang.kind_type_of_frame_kind kind in
             let l = Lang.apply ~t:(Lang.list_t (Lang.source_t kind)) f [] in
             let l = Lang.to_source_list l in
             match l with
               | [] ->
                   ()
               | [s] ->
                   Clock.unify s#clock self#clock ;
                   s#get_ready activation ;
                   self#unregister_source ~already_locked:true ;
                   source <- Some s
               | _ ->
                   assert false))

    (* Source methods: attempt to #select as soon as it could be useful
     * for the selection function to change the source. *)
    method private wake_up ancestors =
      activation <- (self :> Source.source) :: ancestors ;
      Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f ;
      self#select

    method private sleep =
      Lang.iter_sources
        (fun s -> s#leave ~dynamic:true (self :> Source.source))
        f ;
      self#unregister_source ~already_locked:false

    method is_ready =
      self#select ;
      match source with Some s when s#is_ready -> true | _ -> false

    method private get_frame frame =
      begin
        match source with Some s -> s#get frame | None ->
            Frame.add_break frame (Frame.position frame)
      end ;
      self#select

    method remaining = match source with Some s -> s#remaining | None -> -1

    method abort_track =
      match source with Some s -> s#abort_track | None -> ()

    method seek n = match source with Some s -> s#seek n | None -> 0

    method self_sync =
      match source with Some s -> s#self_sync | None -> false
  end

let () =
  let k = Lang.univ_t () in
  Lang.add_operator "source.dynamic"
    [("", Lang.fun_t [] (Lang.list_t (Lang.source_t k)), None, None)]
    ~kind:(Lang.Unconstrained k)
    ~descr:"Dynamically change the underlying source."
    ~category:Lang.TrackProcessing ~flags:[Lang.Experimental]
    (fun p kind -> new dyn ~kind (List.assoc "" p))
