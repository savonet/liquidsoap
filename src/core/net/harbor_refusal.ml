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

type t = { status : int; reason : string; headers : (string * string) list }

let header_line (name, value) = name ^ ": " ^ value ^ "\r\n"

let reply ~protocol { status; reason; headers } =
  Printf.sprintf "HTTP/%s %d %s\r\n%sContent-Length: 0\r\n\r\n" protocol status
    reason
    (String.concat "" (List.map header_line headers))

let is_token name =
  name <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
         | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^'
         | '_' | '`' | '|' | '~' ->
             true
         | _ -> false)
       name

let has_line_break = String.exists (fun c -> c = '\r' || c = '\n')

let header_error (name, value) =
  if not (is_token name) then Some "Header name must be an HTTP token."
  else if has_line_break value then
    Some "Header value must not contain a line break."
  else None

let error { status; reason; headers } =
  if status < 300 || status > 599 then
    Some "Refusal status must be between 300 and 599."
  else if has_line_break reason then
    Some "Refusal reason must not contain a line break."
  else List.find_map header_error headers

let t =
  Lang.optional_method_t
    (Lang.record_t [("status", Lang.int_t); ("reason", Lang.string_t)])
    [
      ( "headers",
        ([], Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)),
        "Reply headers." );
    ]

let of_value value =
  let fields, _ = Lang.split_meths value in
  let refusal =
    {
      status = Lang.to_int (List.assoc "status" fields);
      reason = Lang.to_string (List.assoc "reason" fields);
      headers =
        (match List.assoc_opt "headers" fields with
          | None -> []
          | Some headers ->
              List.map
                (fun header ->
                  let name, value = Lang.to_product header in
                  (Lang.to_string name, Lang.to_string value))
                (Lang.to_list headers));
    }
  in
  match error refusal with
    | Some message -> raise (Error.Invalid_value (value, message, []))
    | None -> refusal
