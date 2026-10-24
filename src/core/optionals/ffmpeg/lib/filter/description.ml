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

type mode = [ `Probe | `Build ]

type element = {
  eid : int;
  name : string;
  mutable attached : [ `Attached ] Avfilter.filter option;
}

type media = [ `Audio | `Video ]

type key = {
  key_element : int;
  key_direction : [ `Input | `Output ];
  key_media : media;
  key_index : int;
}

type t = {
  gid : int;
  mutable statements : (mode -> Avfilter.config -> unit) list;
  mutable edges : (int * int) list;
  mutable next : int;
  mutable probe : Avfilter.config option;
  unlinked : (key, string) Hashtbl.t;
}

type 'm pad = {
  pid : int;
  owner : t;
  element : element;
  media : media;
  index : int;
  resolve : unit -> ([ `Attached ], 'm, [ `Output ]) Avfilter.pad;
}

let values = Atomic.make 0
let creation_number () = Atomic.fetch_and_add values 1

let create ~gid () =
  {
    gid;
    statements = [];
    edges = [];
    next = 0;
    probe = Some (Avfilter.init ());
    unlinked = Hashtbl.create 10;
  }

let gid t = t.gid
let in_scope t = t.probe <> None
let close t = t.probe <- None

let element t prefix =
  let eid = t.next in
  t.next <- eid + 1;
  { eid; name = Printf.sprintf "%s_%d" prefix eid; attached = None }

let attached e =
  match e.attached with
    | Some filter -> filter
    | None -> failwith ("Filter instance not attached: " ^ e.name)

let add t statement =
  match t.probe with
    | None -> invalid_arg "Description.add: graph out of scope"
    | Some config ->
        statement `Probe config;
        t.statements <- statement :: t.statements

let replay t config =
  List.iter (fun statement -> statement `Build config) (List.rev t.statements)

let media_name = function `Audio -> "audio" | `Video -> "video"

let expect t (e : element) direction media index pad_name =
  Hashtbl.replace t.unlinked
    {
      key_element = e.eid;
      key_direction = direction;
      key_media = media;
      key_index = index;
    }
    (Printf.sprintf "%s %s pad %d (%s) of %s"
       (match direction with `Input -> "input" | `Output -> "output")
       (media_name media) index pad_name e.name)

let consumed t direction (e : element) media index =
  Hashtbl.remove t.unlinked
    {
      key_element = e.eid;
      key_direction = direction;
      key_media = media;
      key_index = index;
    }

let expect_pads t e (filter : [ `Attached ] Avfilter.filter) =
  let each direction media pads =
    List.iteri
      (fun i pad -> expect t e direction media i (Avfilter.pad_name pad))
      pads
  in
  each `Input `Audio filter.io.inputs.audio;
  each `Input `Video filter.io.inputs.video;
  each `Output `Audio filter.io.outputs.audio;
  each `Output `Video filter.io.outputs.video

let first_unlinked t =
  Hashtbl.fold
    (fun key descr found ->
      match found with
        | Some (k, _) when compare k key <= 0 -> found
        | _ -> Some (key, descr))
    t.unlinked None
  |> Option.map snd

let edge t ~src ~dst = t.edges <- (src.eid, dst.eid) :: t.edges

let reached t ~from =
  let seen = Hashtbl.create 10 in
  let rec visit eid =
    if not (Hashtbl.mem seen eid) then (
      Hashtbl.replace seen eid ();
      List.iter (fun (s, d) -> if s = eid then visit d) t.edges)
  in
  List.iter (fun (e : element) -> visit e.eid) from;
  fun (e : element) -> Hashtbl.mem seen e.eid

let pad t element media index resolve =
  { pid = creation_number (); owner = t; element; media; index; resolve }

let audio_output t e index =
  pad t e `Audio index (fun () -> List.nth (attached e).io.outputs.audio index)

let video_output t e index =
  pad t e `Video index (fun () -> List.nth (attached e).io.outputs.video index)

let link_error (src : _ pad) dst_name message =
  Printf.sprintf "Could not link %s:%d to %s: %s" src.element.name src.index
    dst_name message

let link t (src : 'm pad) ~dst ~dst_name
    (dst_pad : unit -> ([ `Attached ], 'm, [ `Input ]) Avfilter.pad) =
  consumed t `Output src.element src.media src.index;
  edge t ~src:src.element ~dst;
  add t (fun _ _ ->
      try Avfilter.link (src.resolve ()) (dst_pad ())
      with Avutil.Error e ->
        failwith (link_error src dst_name (Avutil.string_of_error e)))
