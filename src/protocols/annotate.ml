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

(* The annotate protocol allows to set the initial metadata for a request:
 * annotate:key1=val1,key2=val2,...:uri
 * is resolved into uri, and adds the bindings to the request metadata.
 * The values can be "strings", or directly integers, floats or identifiers. *)

open Genlex

exception Error

let annotate s ~log _ =
  try
    (* Avoid =- being lexed as a single identifier. *)
    (* TODO: we should really code a custom lexer instead of having such hacks *)
    let s = Pcre.substitute ~pat:"=-" ~subst:(fun _ -> "= -") s in
    let l = String.length s in
    let pos = ref 0 in
    let str =
      Stream.from (fun i ->
          pos := i;
          if i < l then Some s.[i] else None)
    in
    let lexer = make_lexer [":"; ","; "="] str in
    let rec parse metadata =
      match Stream.next lexer with
        | Kwd ":" ->
            let uri = String.sub s !pos (l - !pos) in
            (* Revert the above hack. *)
            let uri = Pcre.substitute ~pat:"= -" ~subst:(fun _ -> "=-") uri in
            (metadata, uri)
        | Kwd "," -> parse metadata
        | Ident key ->
            if key <> "" && key.[0] = ':' then (
              let uri =
                String.sub key 1 (String.length key - 1)
                ^ String.sub s !pos (l - !pos)
              in
              (* Revert the above hack. *)
              let uri = Pcre.substitute ~pat:"= -" ~subst:(fun _ -> "=-") uri in
              (metadata, uri) )
            else (
              match Stream.next lexer with
                | Kwd "=" -> (
                    match Stream.next lexer with
                      | String s -> parse ((key, s) :: metadata)
                      | Int i -> parse ((key, string_of_int i) :: metadata)
                      | Float f -> parse ((key, string_of_float f) :: metadata)
                      | Ident k -> parse ((key, k) :: metadata)
                      | _ -> raise Error )
                | _ -> raise Error )
        | _ -> raise Error
    in
    let metadata, uri = parse [] in
    [Request.indicator ~metadata:(Utils.hashtbl_of_list metadata) uri]
  with Error | Stream.Failure | Stream.Error _ ->
    log "annotate: syntax error";
    []

let () =
  Lang.add_protocol ~doc:"Add metadata to a request"
    ~syntax:"annotate:key=\"val\",key2=\"val2\",...:uri" ~static:false
    "annotate" annotate
