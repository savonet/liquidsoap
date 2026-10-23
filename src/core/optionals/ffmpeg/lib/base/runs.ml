(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

module type Item = sig
  type t

  val copy : t -> t
  val checksum : t -> string
end

module type Params = sig
  type t

  val known : t -> bool
end

module type S = sig
  type item
  type params

  type run = {
    length : int;
    source_id : Source_id.t;
    continues : bool;
    time_base : Avutil.rational;
    source_params : Copy_params.t option;
    items : (int * item) list;
  }

  type data = {
    mutable params : params;
    mutable length : int;
    mutable runs : (int * run) list;
  }

  include
    Content_base.DataSpecs with type params := params and type data := data

  val run :
    ?continues:bool ->
    ?source_params:Copy_params.t ->
    source_id:Source_id.t ->
    time_base:Avutil.rational ->
    length:int ->
    (int * item) list ->
    run

  val of_run : params -> run -> data
end

module Make (P : Params) (I : Item) = struct
  type item = I.t
  type params = P.t

  type run = {
    length : int;
    source_id : Source_id.t;
    continues : bool;
    time_base : Avutil.rational;
    source_params : Copy_params.t option;
    items : (int * item) list;
  }

  type data = {
    mutable params : params;
    mutable length : int;
    mutable runs : (int * run) list;
  }

  let run ?(continues = false) ?source_params ~source_id ~time_base ~length
      items =
    {
      length;
      source_id;
      continues;
      time_base;
      source_params;
      items = List.stable_sort (fun (p, _) (p', _) -> compare p p') items;
    }

  let make ?(length = 0) params = { params; length; runs = [] }

  let of_run params (run : run) =
    { params; length = run.length; runs = [(0, run)] }

  let length (d : data) = d.length
  let params d = d.params

  let sub_run (run : run) offset len =
    {
      run with
      length = len;
      items =
        List.filter_map
          (fun (pos, item) ->
            if offset <= pos && pos < offset + len then
              Some (pos - offset, I.copy item)
            else None)
          run.items;
    }

  let same_source_params a b =
    match (a, b) with
      | None, None -> true
      | Some a, Some b -> Copy_params.equal a b
      | _ -> false

  let continues (start, (r : run)) (start', (r' : run)) =
    start + r.length = start'
    && r.source_id = r'.source_id && r.time_base = r'.time_base
    && same_source_params r.source_params r'.source_params

  let collapse runs =
    let rec go acc = function
      | [] -> List.rev acc
      | ((_, (r' : run)) as next) :: rest -> (
          match acc with
            | ((start, (r : run)) as prev) :: acc when continues prev next ->
                let items =
                  r.items
                  @ List.map
                      (fun (pos, item) -> (pos + r.length, item))
                      r'.items
                in
                go
                  ((start, { r with length = r.length + r'.length; items })
                  :: acc)
                  rest
            | _ -> go (next :: acc) rest)
    in
    go [] runs

  (* The parts of [runs] outside [[from, until)], each a sub-run at its own
     absolute start. *)
  let cut runs ~from ~until =
    List.concat_map
      (fun (start, (r : run)) ->
        let stop = start + r.length in
        if stop <= from || until <= start then [(start, r)]
        else
          (if start < from then [(start, sub_run r 0 (from - start))] else [])
          @
          if until < stop then
            [(until, sub_run r (until - start) (stop - until))]
          else [])
      runs

  let blit (src : data) src_pos (dst : data) dst_pos len =
    let len = max 0 (min len (src.length - src_pos)) in
    let src_stop = src_pos + len in
    let copied =
      List.filter_map
        (fun (start, (r : run)) ->
          let stop = start + r.length in
          let from = max start src_pos and until = min stop src_stop in
          if from < until then
            Some
              ( dst_pos + (from - src_pos),
                sub_run r (from - start) (until - from) )
          else None)
        src.runs
    in
    let kept = cut dst.runs ~from:dst_pos ~until:(dst_pos + len) in
    dst.runs <-
      collapse
        (List.stable_sort (fun (s, _) (s', _) -> compare s s') (kept @ copied));
    dst.length <- max dst.length (dst_pos + len);
    if not (P.known dst.params) then dst.params <- src.params

  let copy (d : data) =
    {
      d with
      runs =
        List.map
          (fun (start, r) ->
            ( start,
              {
                r with
                items = List.map (fun (pos, item) -> (pos, I.copy item)) r.items;
              } ))
          d.runs;
    }

  let checksum (d : data) =
    let run_checksum (start, (r : run)) =
      Printf.sprintf "%d+%d:%d:%d/%d:%s" start r.length r.source_id
        r.time_base.Avutil.num r.time_base.Avutil.den
        (String.concat "|"
           (List.map
              (fun (pos, item) -> Printf.sprintf "%d=%s" pos (I.checksum item))
              r.items))
    in
    Digest.to_hex
      (Digest.string
         (String.concat "||"
            (string_of_int d.length :: List.map run_checksum d.runs)))
end
