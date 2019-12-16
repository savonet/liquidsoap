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

let log = Log.make ["decoder"; "puremeta"]

exception Error

(** Parse next item:
  *   [[Int:]Int:]Int String String
  * Int can be Float, and there can be more than hours. *)
let parse_item stream =
  let rec ts_of_list d = function
    | hd :: tl -> (hd *. d) +. ts_of_list (d *. 60.) tl
    | [] -> 0.
  in
  let rec read_meta timestamp =
    match Stream.next stream with
      | Genlex.Kwd ":" -> read_meta timestamp
      | Genlex.Int i -> read_meta (float i :: timestamp)
      | Genlex.Float f -> read_meta (f :: timestamp)
      | Genlex.String key -> (
          match Stream.next stream with
            | Genlex.String value -> (ts_of_list 1. timestamp, key, value)
            | _ -> raise Error )
      | _ -> raise Error
  in
  read_meta []

let parse_file filename =
  let input = open_in filename in
  let lexer = Genlex.make_lexer [":"] (Stream.of_channel input) in
  let rec parse acc =
    match try Some (parse_item lexer) with Stream.Failure -> None with
      | Some i -> parse (i :: acc)
      | None -> List.rev acc
  in
  let data = parse [] in
  close_in input;
  data

let empty = { Frame.audio = 0; video = 0; midi = 0 }

let file_deco filename =
  let events = ref (parse_file filename) in
  let t = ref 0. in
  let size = Lazy.force Frame.size in
  let fill frame =
    let pos = Frame.position frame in
    let duration = Frame.seconds_of_master (size - pos) in
    let rec aux () =
      match !events with
        | (ts, k, v) :: tl ->
            if ts < !t +. duration then (
              let pos = Frame.master_of_seconds (ts -. !t) in
              let meta = Hashtbl.create 1 in
              Hashtbl.add meta k v;
              Frame.set_metadata frame pos meta;
              events := tl;
              aux () )
            else Frame.add_break frame size
        | [] -> Frame.add_break frame pos
    in
    ignore (Frame.content_of_type frame pos empty);
    aux ();
    t := !t +. Frame.seconds_of_master (Frame.position frame - pos);
    -1
    (* TODO remaining time *)
  in
  { Decoder.fill; fseek = (fun _ -> 0); close = ignore }

let () =
  Decoder.file_decoders#register "META" (fun ~metadata:_ filename kind ->
      if Frame.type_has_kind empty kind then (
        ignore (parse_file filename);
        Some (fun () -> file_deco filename) )
      else None)
