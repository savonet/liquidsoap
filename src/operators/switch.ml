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

(** Abstract operator which selects one of its children sources
  * either at the beginning of a track or at every frame,
  * depending on a parametrizable predicate.
  * A few specializations of it are defined below. *)

open Types
open Dtools

(* A transition is a value of type (source,source) -> source *)
type transition = Lang.value

type child = {
  source     : source ;
  transition : transition
}

(** The switch can either happen at any time in the stream (insensitive)
  * or only at track limits (sensitive), in which case it is possible
  * to have the transition being triggered a while before end of track --
  * anticipation parameter specified as a number of frames. *)
type track_mode = Sensitive of int | Insensitive

class virtual switch ?(mode=Sensitive 0) (cases : child list) =
object (self)

  inherit operator (List.map (fun x -> x.source) cases) as super

  (** The selection method should return None or Some c,
    * where c is a ready child. *)
  method virtual private select : child option

  val mutable selected = None
  val mutable to_finish = []

  method after_output =
    self#clear_cache ;
    List.iter (fun s -> s#after_output) to_finish ;
    to_finish <- [] ;
    match selected with
      | Some (_,s) -> s#after_output
      | None -> ()

  val mutable activation = []

  method wake_up activator =
    activation <- (self:>source)::activator ;
    List.iter
      (fun { transition = transition ; source = s } ->
         s#get_ready ~dynamic:true activation ;
         Lang.iter_sources
           (fun s -> s#get_ready ~dynamic:true activation)
           transition)
      cases

  method sleep =
    List.iter
      (fun { transition = transition ; source = s } ->
         s#leave ~dynamic:true (self:>source) ;
         Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>source))
           transition)
      cases

  method is_ready = self#select <> None

  method get_frame ab =
    let reselect ?(forget=false) () = match self#select with
      | Some c ->
          begin match selected with
            | None ->
                self#log 3 (Log.f "switch to %s" c.source#id) ;
                (* The source is already ready, this call is only there for
                 * allowing an uniform treatment of switches, triggering
                 * a #leave call. *)
                c.source#get_ready activation ;
                selected <- Some (c,c.source)
            | Some (old_c,old_s) when old_c != c ->
                self#log 3 
                  (Log.f
                     "switch to %s with%s transition"
                     c.source#id
                     (if forget then " forgetful" else "")) ;
                old_s#leave (self:>source) ;
                to_finish <- old_s::to_finish ;
                let old_source =
                  if forget then Blank.empty else old_c.source
                in
                let s =
                  Lang.to_source
                    (Lang.apply c.transition
                       [ "",Lang.source old_source ; "",Lang.source c.source ])
                in
                  s#get_ready activation ;
                  selected <- Some (c,s)
            | _ -> ()
          end
      | None ->
          begin match selected with
            | Some (old_c,old_s) ->
                old_s#leave (self:>source) ;
                to_finish <- old_s::to_finish ;
                selected <- None
            | None -> ()
          end
    in
      match selected with
      | None ->
          reselect () ;
          if selected <> None then self#get_frame ab ;
      | Some (_,s) ->
          s#get ab ;
          begin match mode with
            | Insensitive -> reselect ()
            | Sensitive d ->
                if Mixer.Buffer.is_partial ab then
                  reselect ~forget:true ()
                else
                  if s#remaining >0 && s#remaining <= d then reselect ()
          end

  method abort_track =
    match selected with
      | Some (_,s) -> s#abort_track
      | None -> ()

end

(** Common tools for Lang bindings of switch operators *)

let common = [
  "track_sensitive", Lang.bool_t, Some (Lang.bool true),
  Some "Re-select only on end of tracks" ;

  "before", Lang.float_t, Some (Lang.float 0.),
  Some ("EXPERIMENTAL: for track_sensitive switches, "^
        "trigger transitions before the end of track.") ;

  "transitions",
  Lang.list_t
    (Lang.fun_t [ false,"",Lang.source_t ;
                  false,"",Lang.source_t ] Lang.source_t),
  Some (Lang.list []),
  Some "Transition functions, padded with (fun (x,y) -> y) functions."
]

let default_transition =
  Lang.val_fun [ "","x",Lang.source_t,None ;
                 "","y",Lang.source_t,None ] (Lang.var "y")

let extract_common p l =
  let ts =
    let before = Lang.to_float (List.assoc "before" p) in
      if Lang.to_bool (List.assoc "track_sensitive" p) then
        Sensitive (int_of_float (before /. Mixer.Buffer.length))
      else
        Insensitive
  in
  let tr =
    let tr = Lang.to_list (List.assoc "transitions" p) in
    let ltr = List.length tr in
      if ltr > l then raise (Lang.Invalid_value
                               ((List.assoc "transitions" p),
                                "Too many transitions")) ;
      if ltr < l then
        tr @ (Utils.make_list (l-ltr) default_transition)
      else
        tr
  in
    ts,tr

(** Switch: switch according to user-defined predicates. *)

let satisfied f = Lang.to_bool (Lang.apply f [])
let trivially_true = function
  | { Lang.value = Lang.Fun (_, { Lang.value = Lang.Bool true }) } -> true
  | _ -> false

let third (_,_,s) = s

class lang_switch mode
  ?(strict=false) (children : (Lang.value * bool * child) list) =
object (self)

  inherit switch ~mode (List.map third children)

  (* We need to override #is_ready to deal with strictness and call
   * the select method in a way distinguishable from a proper selection. *)
  method is_ready =
    if not strict && selected <> None then true else
      self#my_select ~ignore_single:true <> None

  (* It is useful to be able to ignore the single flag, when checking if the
   * source is ready, in the strict case. Then, we must check that the current
   * source still has a valid scheduling interval, without bothering with
   * single constraints, which are only used for real reselections. *)
  method private my_select ~ignore_single =
    let selected s =
      match selected with
        | Some (child,_) when child=s -> true
        | _ -> false
    in try
      Some (third (List.find
                   (fun (d,single,s) ->
                      (* Check single constraints *)
                      (if selected s then
                         ignore_single || not single
                       else
                         true) &&
                      (satisfied d) && s.source#is_ready)
                   children))
    with
      | Not_found -> None

  method select = self#my_select ~ignore_single:false

  method stype =
    if List.exists
         (fun (d,single,s) ->
            s.source#stype = Infallible &&
            not single &&
            trivially_true d)
         children
    then Infallible else Fallible

end

let _ =
  let pred_t = Lang.fun_t [] Lang.bool_t in
  let proto = 
    [ "strict", Lang.bool_t, Some (Lang.bool false),
      Some ("Unset the operator's ready flag as soon as there is "^
            "no valid interval, possibly interrupting ongoing tracks.") ;
      "single", Lang.list_t Lang.bool_t, Some (Lang.list []),
      Some ("Forbid the selection of a branch for two tracks in a row. "^
            "The empty list stands for [false,...,false].") ;
      "", Lang.list_t (Lang.product_t pred_t Lang.source_t), None,
      Some "Sources Si with the interval Ii when they should be played." ]
  in
    Lang.add_operator "switch"
      ~descr:("At the beginning of a track, select the first source Si "^
              "such than the temporal predicate Ii is true.")
      (common@proto)
      (fun p ->
         let children =
           List.map
             (fun p ->
                let (pred,s) = Lang.to_product p in
                  pred, Lang.to_source s)
           (Lang.to_list (List.assoc "" p))
         in
         let ts,tr = extract_common p (List.length children) in
         let strict = Lang.to_bool (List.assoc "strict" p) in
         let singles =
           List.map Lang.to_bool (Lang.to_list (List.assoc "single" p))
         in
         let singles =
           if singles = [] then
             Utils.make_list (List.length children) false
           else
             singles
         in
         let children =
           List.map2 (fun t (f,s) -> f,{ source = s ; transition = t })
             tr children
         in
         let children =
           try List.map2 (fun (d,s) single -> d,single,s) children singles with
             | Invalid_argument("List.map2") ->
                 raise (Lang.Invalid_value
                          (List.assoc "single" p,
                           "there should be exactly one flag per children"))
         in
           ((new lang_switch ~strict ts children):>source))

(** Fallback selector: switch to the first ready source. *)
class fallback mode children =
object

  inherit switch ~mode children

  method private select =
    try Some (List.find (fun s -> s.source#is_ready) children) with
      | Not_found -> None

  method stype =
    if List.exists (fun s -> s.source#stype = Infallible) children then
      Infallible
    else
      Fallible

end

let _ =
  let proto =
    [ "",
      Lang.list_t Lang.source_t,
      None,
      Some "Select the first ready source in this list." ]
  in
    Lang.add_operator "fallback"
      ~descr:"At the beginning of each track, select the first ready child."
      (common@proto)
      (fun p ->
         let children = Lang.to_source_list (List.assoc "" p) in
         let ts,tr = extract_common p (List.length children) in
         let children = List.map2 (fun t s -> { transition = t ;
                                                source = s }) tr children in
           ((new fallback ts children):>source))

(** Random switch *)
exception Found of child
class random mode children =
object
  inherit switch ~mode (List.map snd children)

  method private select =
    let (ready_list,n) =
      List.fold_left
        (fun (l,k) (w,s) -> if s.source#is_ready then (s,w)::l,(k+w) else l,k)
        ([],0) children
    in
      if n = 0 then None else
        try
          let sel = Random.int n in
            ignore (List.fold_left
                      (fun k (s,w) ->
                         if k+w > sel then raise (Found s) else (k+w))
                      0 ready_list) ;
            assert false
        with
          | Found s -> Some s

  method stype =
    if List.exists (fun (_,s) -> s.source#stype = Infallible) children then
      Infallible
    else
      Fallible
end

let _ =
    Lang.add_operator "random"
      (common @
       [ "weights", Lang.list_t Lang.int_t, Some (Lang.list []),
         Some "Weights of the children in the choice." ;
         "", Lang.list_t Lang.source_t, None, None ])
      ~descr:"At the beginning of every track, select a random ready child."
      (fun p ->
         let children = Lang.to_source_list (List.assoc "" p) in
         let ts,tr = extract_common p (List.length children) in
         let weights =
           List.map Lang.to_int (Lang.to_list (List.assoc "weights" p))
         in
         let weights =
           if weights <> [] then weights else
             Utils.make_list (List.length children) 1
         in
         let children =
           List.map2
             (fun w c -> w,c)
             weights
             (List.map2 (fun tr s -> { transition = tr ; source = s })
                tr children)
         in
           ((new random ts children):>source)) ;
