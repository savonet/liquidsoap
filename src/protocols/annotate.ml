(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
 * annotate:key1=val1,key2=val2,...:uri
 * is resolved into uri, and adds the bindings to the request metadata.
 * The values can be "strings", or directly integers, floats or identifiers. *)

exception Error

let kwd = [':';',';'=']
let rec lex s =
  let rec aux o =
    if o >= String.length s then [] else
      if List.mem s.[o] kwd then
        let k = `Kwd (String.make 1 s.[o]) in
        if s.[o] = ':' then
          [k; `Ident (String.sub s (o+1) (String.length s-o-1))]
        else
          k::(aux (o+1))
      else
        let n = ref 0 in
        while o + !n < String.length s && not (List.mem s.[o + !n] kwd) do
          incr n
        done;
        let i = `Ident (String.sub s o !n) in
        i::(aux (o + !n))
  in
  aux 0

let annotate s ~log maxtime =
  try
    let s = lex s in
    let rec parse = function
      | [`Kwd ":";`Ident file] -> [],file
      | (`Kwd ",")::md -> parse md
      | (`Ident key)::(`Kwd "=")::(`Ident value)::md ->
        let md, file = parse md in
        (key,value)::md, file
      | _ -> raise Error
    in
    let metadata,uri = parse s in
    [Request.indicator ~metadata:(Utils.hashtbl_of_list metadata) uri]
  with
  | Error
  | Stream.Failure | Stream.Error _ -> log "annotate: syntax error" ; []

let () =
  Request.protocols#register "annotate"
    ~sdoc:("[annotate:key=\"val\",key2=\"val2\",...:uri] adds "^
           "the metadata to the request and is then resolved into uri")
    { Request.resolve = annotate ; Request.static = false }
