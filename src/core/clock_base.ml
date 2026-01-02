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

exception Invalid_state

module Evaluation = Liquidsoap_lang.Evaluation

type active_source = < id : string ; reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of active_source ]

type sync_source = ..
type self_sync = [ `Static | `Dynamic ] * sync_source option

module Queue = struct
  include Queues.Queue

  let push q v = if not (exists q (fun v' -> v == v')) then push q v
end

module WeakQueue = struct
  include Queues.WeakQueue

  let push q v = if not (exists q (fun v' -> v == v')) then push q v
end

exception Sync_source_name of string

let sync_sources_handlers = Queue.create ()

let string_of_sync_source s =
  try
    Queue.iter sync_sources_handlers (fun fn -> fn s);
    assert false
  with Sync_source_name s -> s

module type SyncSource = sig
  type t

  val to_string : t -> string
end

module MkSyncSource (S : SyncSource) = struct
  type sync_source += Sync_source of S.t

  let make v = Sync_source v

  let () =
    Queue.push sync_sources_handlers (function
      | Sync_source v -> raise (Sync_source_name (S.to_string v))
      | _ -> ())
end

type sync_source_entry = {
  name : string;
  sync_source : sync_source;
  stack : Pos.t list;
}

type clock_sync_error = {
  name : string;
  stack : Pos.t list;
  sync_sources : sync_source_entry list;
}

exception Sync_error of clock_sync_error

let () =
  Printexc.register_printer (function
    | Sync_error { name; stack; sync_sources } ->
        let buf = Buffer.create 1024 in
        let formatter = Format.formatter_of_buffer buf in
        let open Liquidsoap_lang in
        let print_stack = function
          | [] -> " Unknown position"
          | l ->
              let l = List.init (min (List.length l) 3) (List.nth l) in
              String.concat "\n"
                (List.map (fun p -> " " ^ Liquidsoap_lang.Pos.to_string p) l)
        in
        ignore (print_stack []);
        let pos = match stack with [] -> None | p :: _ -> Some p in
        Runtime.error_header ~formatter 17 pos;
        Format.fprintf formatter
          "%s has multiple synchronization sources. Do you need to set \
           self_sync=false?@."
          name;
        Format.fprintf formatter "\nSync sources:\n";
        List.iter
          (fun { name; sync_source } ->
            Format.fprintf formatter " %s from source %s\n"
              (string_of_sync_source sync_source)
              name)
          sync_sources;
        Format.fprintf formatter "\nStack traces:\n";
        Format.fprintf formatter "%s:\n%s\n\n" name (print_stack stack);
        List.iter
          (fun { name; stack; sync_source = _ } ->
            Format.fprintf formatter "%s:\n%s\n\n" name (print_stack stack))
          sync_sources;
        Format.fprintf formatter "@]@.";
        Format.pp_print_flush formatter ();
        Some (Buffer.contents buf)
    | _ -> None)

type activation = < id : string ; sleep : activation -> unit >

type source =
  < id : string
  ; stack : Pos.t list
  ; self_sync : self_sync
  ; source_type : source_type
  ; active : bool
  ; activations : activation list
  ; wake_up : activation -> unit
  ; sleep : activation -> unit
  ; is_ready : bool
  ; get_frame : Frame.t >

let self_sync_type sources =
  Lazy.from_fun (fun () ->
      if List.exists (fun s -> fst s#self_sync = `Dynamic) sources then `Dynamic
      else `Static)

let self_sync sources =
  let self_sync_type = self_sync_type sources in
  fun ~source () ->
    let sync_sources =
      List.fold_left
        (fun sync_sources s ->
          if s#is_ready then (
            match s#self_sync with
              | _, Some sync_source ->
                  { name = s#id; stack = s#stack; sync_source } :: sync_sources
              | _ -> sync_sources)
          else sync_sources)
        [] sources
    in
    match
      List.sort_uniq
        (fun { sync_source = s } { sync_source = s' } -> Stdlib.compare s s')
        sync_sources
    with
      | [] -> (Lazy.force self_sync_type, None)
      | [{ sync_source }] -> (Lazy.force self_sync_type, Some sync_source)
      | sync_sources ->
          raise
            (Sync_error { name = source#id; stack = source#stack; sync_sources })
