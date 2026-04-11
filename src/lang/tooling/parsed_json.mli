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

open Liquidsoap_lang

val to_json : Parsed_term.t -> Json.t

(** Canonical JSON representation of a parsed term.

    Every node carries a [position] field: [[start, end]] where each position
    object has [fname], [lnum], [bol], [cnum] fields ([cnum] is the absolute
    character offset from the start of the file).

    Block bodies (program, def, fun, rfun, simple_fun, while, for, iterable_for,
    block) are emitted as flat arrays in a [body] field.

    Block positions: for [while]/[for]/[iterable_for] and [try], block positions
    span from the opening keyword to the start of the next keyword (e.g. from
    [do] to [end]). For [if], [then_block.position] spans from [then] to the
    next [elsif]/[else]/[end]; [else_block.position] spans from [else] to [end].
    This lets prettier attach comments between keywords to the correct block.

    Specific node shapes:

    - [program]: [{type:"program", position, body:[...stmts]}]
    - [def]:
      [{type:"def", position, decoration, pat, arglist, cast, body:[...stmts]}]
    - [let]:
      [{type:"let", position, decoration, pat, arglist, cast, definition}] The
      [definition] field holds the right-hand side expression.
    - [binding]: [{type:"binding", position, pat, arglist, cast, definition}]
    - [fun]: [{type:"fun", position, arguments:[...], body:[...stmts]}]
    - [rfun]: [{type:"rfun", position, name, arguments:[...], body:[...stmts]}]
    - [simple_fun]: [{type:"simple_fun", position, body:[...stmts]}]
    - [if]:
      [{type:"if", position, condition, then_block:{type:"then_block", position,
       body:[...stmts]}, elsif:[{type:"elsif", position, condition,
       body:[...stmts]}, ...], else_block:{type:"else_block", position,
       body:[...stmts]}|null}] [then_block.position] starts at the [then]
      keyword; [else_block.position] starts at the [else] keyword; each
      [elsif.position] starts at its [elsif] keyword. These block nodes are
      comment-attachment anchors.
    - [if_def] / [if_version] / [if_encoder]:
      [{type:"if_def", position, negative, condition,
       then_block:{type:"ifdef_block", position, body:[...stmts]},
       else_block:{type:"ifdef_block", position, body:[...stmts]}|null}] Block
      positions span from the first statement to the start of [%else]/[%end].
      The [ifdef_block] type signals no indentation.
    - [while]:
      [{type:"while", position, parts:[{type:"while_header", position,
       condition}, {type:"while_body",   position, body:[...stmts]}]}]
    - [for]:
      [{type:"for", position, parts:[{type:"for_header", position, variable,
       from, to}, {type:"for_body",   position, body:[...stmts]}]}]
    - [iterable_for]:
      [{type:"iterable_for", position, parts:[{type:"iterable_for_header",
       position, variable, iterator}, {type:"iterable_for_body",   position,
       body:[...stmts]}]}]
    - [try]:
      [{type:"try", position, parts:[{type:"try_body",    position,
       body:[...stmts]}, {type:"try_catch",   position, variable, errors_list,
       body:[...stmts]},  (* optional *) {type:"try_finally", position,
       body:[...stmts]}]}] (* optional *)
    - [block]: [{type:"block", position, body:[...stmts]}]
    - [inline_if]: [{type:"inline_if", position, condition, then, else}]
      (ternary; [then] and [else] are single expression nodes, not arrays) *)
val to_json_canonical : Parsed_term.t -> Json.t

(** [parse_string ?formatter content] parses [content] and returns a JSON object
    with two fields:
    - ["ast"]: a program node in the canonical format (see [to_json_canonical])
    - ["comments"]: an array of [{start, end, value}] objects where [start] and
      [end] are absolute character offsets ([pos_cnum]) and [value] is the raw
      comment text (including the leading [#]). *)
val parse_string : ?formatter:Format.formatter -> string -> Json.t
