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

(** Abstract operator which selects one of its children sources
  * either at the beginning of a track or at every frame,
  * depending on a parametrizable predicate.
  * A few specializations of it are defined below. *)

open Source
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
  * anticipation parameter specified as a number of ticks. *)
type track_mode = Sensitive of int | Insensitive

class virtual switch ?(mode=Sensitive 0) (cases : child list) =
object (self)

  inherit operator (List.map (fun x -> x.source) cases) as super

  val mutable selected = None
  val mutable to_finish = []

  (** Indicates that the former child was left without having finished its
    * track. *)
  val mutable need_eot = false

  (** The selection method should return None or Some c,
    * where c is a ready child. *)
  method virtual private select : child option

  (** Don't call #select directly but use #cached_select
    * to ensure consistency during one time tick between #is_ready and
    * #get_frame. *)
  val mutable cached_selected = None
  method private cached_select =
    match cached_selected with
      | Some c -> c
      | None ->
          let c = self#select in
            cached_selected <- Some c ;
            c

  method after_output =
    self#advance ;
    List.iter (fun s -> s#after_output) to_finish ;
    to_finish <- [] ;
    List.iter (fun s -> s.source#after_output) cases ;
    (* Selection may have been triggered by a call to #is_ready, without
     * any call to #get_ready (in particular if #select returned None).
     * It is cleared here in order to get a chance to be re-computed later. *)
    cached_selected <- None

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

  method is_ready = need_eot || selected <> None || self#cached_select <> None

  method get_frame ab =
    (* Choose the next child to be played.
     * [forget] tells that the current child has finished its track. *)
    let reselect ?(forget=false) () =
      if not forget then need_eot <- true ;
      match let c = self#cached_select in cached_selected <- None ; c with
      | Some c ->
          begin match selected with
            | None ->
                self#log#f 3 "switch to %s" c.source#id ;
                (* The source is already ready, this call is only there for
                 * allowing an uniform treatment of switches, triggering
                 * a #leave call. *)
                c.source#get_ready activation ;
                selected <- Some (c,c.source)
            | Some (old_c,old_s) when old_c != c ->
                self#log#f 3 "switch to %s with%s transition"
                     c.source#id
                     (if forget then " forgetful" else "") ;
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
            | _ ->
                (* We are staying on the same child,
                 * don't start a new track. *)
                need_eot <- false
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
      (* #select is called only when selected=None, and the cache is cleared
       * as soon as the new selection is set. *)
      assert (selected=None || cached_selected=None) ;
      if need_eot then begin
        need_eot <- false ;
        Frame.add_break ab (Frame.position ab)
      end else
      match selected with
      | None ->
          reselect ~forget:true () ;
          (* Our #is_ready, and caching, ensure the following. *)
          assert (selected <> None) ;
          self#get_frame ab
      | Some (_,s) ->
          s#get ab ;
          if Frame.is_partial ab then
            reselect ~forget:true ()
          else
            if
              match mode with
                | Insensitive -> true
                | Sensitive d -> s#remaining > 0 && s#remaining <= d
            then
              reselect ()

  method remaining =
    match selected with
      | None -> 0
      | Some (_,s) -> s#remaining

  method abort_track =
    match selected with
      | Some (_,s) -> s#abort_track
      | None -> ()

end

(** Common tools for Lang bindings of switch operators *)

let common = [
  "track_sensitive", Lang.bool_t, Some (Lang.bool true),
  Some "Re-select only on end of tracks." ;

  "before", Lang.float_t, Some (Lang.float 0.),
  Some ("EXPERIMENTAL: for track_sensitive switches, "^
        "trigger transitions before the end of track.") ;

  "transitions",
  Lang.list_t
    (Lang.fun_t [ false,"",Lang.source_t ;
                  false,"",Lang.source_t ] Lang.source_t),
  Some (Lang.list []),
  Some "Transition functions, \
        padded with <code>fun (x,y) -> y</code> functions."
]

let default_transition =
  Lang.val_fun [ "","x",None ; "","y",None ] (fun e -> List.assoc "y" e)

let extract_common p l =
  let ts =
    let before = Lang.to_float (List.assoc "before" p) in
      if Lang.to_bool (List.assoc "track_sensitive" p) then
        Sensitive (Fmt.ticks_of_seconds before)
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
  | { Lang.value =
        Lang.Fun (_,_,_,{ Lang_values.term = Lang_values.Bool true }) } -> true
  | _ -> false

let third (_,_,s) = s

class lang_switch mode (children : (Lang.value * bool * child) list) =
object (self)

  inherit switch ~mode (List.map third children)

  method private select =
    let selected s =
      match selected with
        | Some (child,_) when child==s -> true
        | _ -> false
    in try
      Some (third (List.find
                   (fun (d,single,s) ->
                      (* Check single constraints *)
                      (if selected s then
                         not single
                       else
                         true) &&
                      (satisfied d) && s.source#is_ready)
                   children))
    with
      | Not_found -> None

  method stype =
    if List.exists
         (fun (d,single,s) ->
            s.source#stype = Infallible &&
            not single &&
            trivially_true d)
         children
    then Infallible else Fallible

end

let () =
  let pred_t = Lang.fun_t [] Lang.bool_t in
  let proto = 
    [ "single", Lang.list_t Lang.bool_t, Some (Lang.list []),
      Some "Forbid the selection of a branch for two tracks in a row. \
            The empty list stands for <code>[false,...,false]</code>." ;
      "", Lang.list_t (Lang.product_t pred_t Lang.source_t), None,
      Some "Sources with the predicate telling when they can be played." ]
  in
    Lang.add_operator "switch"
      ~category:Lang.TrackProcessing
      ~descr:("At the beginning of a track, select the first source "^
              "whose predicate is true.")
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
           ((new lang_switch ts children):>source))

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

let () =
  let proto =
    [ "",
      Lang.list_t Lang.source_t,
      None,
      Some "Select the first ready source in this list." ]
  in
    Lang.add_operator "fallback" ~category:Lang.TrackProcessing
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
class random strict mode children =
object
  inherit switch ~mode (List.map snd children)

  val mutable pos = -1

  (* Annihilate the reversal in #select once for all. *)
  val children = List.rev children

  method private select =
    let (ready_list,n) =
      List.fold_left
        (fun (l,k) (w,s) -> if s.source#is_ready then (s,w)::l,(k+w) else l,k)
        ([],0) children
    in
      if n = 0 then None else
        try
          let sel = if strict then (pos+1) mod n else Random.int n in
            pos <- sel ;
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

let () =
    Lang.add_operator "random" ~category:Lang.TrackProcessing
      (common @
       [ "weights", Lang.list_t Lang.int_t, Some (Lang.list []),
         Some "Weights of the children in the choice." ;
         "strict", Lang.bool_t, Some (Lang.bool false),
         Some "Do not use random but cycle over the uniform distribution." ;
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
         let strict = Lang.to_bool (List.assoc "strict" p) in
         let children =
           if List.length weights <> List.length children then
             raise
               (Lang.Invalid_value
                  ((List.assoc "weights" p),
                   "there should be as many weights as sources")) ;
           List.map2
             (fun w c -> w,c)
             weights
             (List.map2 (fun tr s -> { transition = tr ; source = s })
                tr children)
         in
           ((new random strict ts children):>source)) ;
