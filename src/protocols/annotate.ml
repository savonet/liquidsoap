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

(* The annotate protocol allows to set the initial metadata for a request:
 * annotate:key1="val1",key2="val2",...:uri
 * is resolved into uri, and adds the bindings to the request metadata. *)

open Genlex
exception Error

let annotate s ~log maxtime =
  try
    let l = String.length s in
    let pos = ref 0 in
    let str =
      Stream.from (fun i ->
                     pos := i ;
                     if i<l then Some s.[i] else None)
    in
    let lexer = make_lexer [":";",";"="] str in
    let rec parse metadata =
      match Stream.next lexer with
        | Kwd ":" -> metadata,(String.sub s !pos (l - !pos))
        | Kwd "," -> parse metadata
        | Ident key ->
            if key<>"" && key.[0]=':' then
              metadata,((String.sub key 1 (String.length key - 1))^
                        (String.sub s !pos (l - !pos)))
            else begin match Stream.next lexer with
              | Kwd "=" -> begin match Stream.next lexer with
                  | String s -> parse ((key,s)::metadata)
                  | _ -> raise Error
                end
              | _ -> raise Error
            end
        | _ -> raise Error
    in
    let metadata,uri = parse [] in
      [Request.indicator ~metadata:(Utils.hashtbl_of_list metadata) uri]
  with
    | Error
    | Stream.Failure -> log "annotate: syntax error" ; []

let () =
  Request.protocols#register "annotate"
    ~sdoc:("[annotate:key=\"val\",key2=\"val2\",...:uri] adds "^
           "the metadata to the request and is then resolved into uri")
    { Request.resolve = annotate ; Request.static = false }
