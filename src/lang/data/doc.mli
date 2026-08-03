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

(** Documentation for everything the language exposes to scripts: builtins and
    operators ({!Value}), plug-in registries ({!Plug}), and the parsing of the
    [##]-style doc comments that feed them.

    This is what [liquidsoap -h], the website reference and the emacs
    completions are generated from. *)

(** Documentation of a builtin or an operator. *)
module Value : sig
  type flag = [ `Deprecated | `Experimental | `Extra | `Hidden ]

  (** What a source-returning operator does, used to group the reference. *)
  type source =
    [ `Audio
    | `Conversion
    | `FFmpegFilter
    | `Fade
    | `Input of [ `Active | `Passive ]
    | `Liquidsoap
    | `MIDI
    | `Output
    | `Synthesis
    | `Testing
    | `Track
    | `Video
    | `Visualization ]

  type category =
    [ `Bool
    | `Configuration
    | `File
    | `Getter
    | `Interaction
    | `Internet
    | `Liquidsoap
    | `List
    | `Math
    | `Metadata
    | `None
    | `Programming
    | `Settings
    | `Source of source
    | `String
    | `System
    | `Time
    | `Track of source ]

  type argument = {
    arg_type : string;
    arg_default : string option;
    arg_description : string option;
  }

  type meth = { meth_type : string; meth_description : string option }

  type t = {
    typ : string;
    category : category;
    flags : flag list;
    description : string;
    examples : string list;  (** Unlabelled arguments are keyed by [None]. *)
    arguments : (string option * argument) list;
    methods : (string * meth) list;
    callbacks : (string * meth) list;
    sync_description : string option;
    composition_description : string option;
    composition : (string * meth) list;
  }

  (** Register documentation under a name. Lazy: building it forces the
      operator's type to be printed, which is wasted work for the common case
      where nobody asks for documentation. *)
  val add : string -> t Lazy.t -> unit

  val count : unit -> int

  (** Print the documentation of one item, as [liquidsoap -h] does. *)
  val print : string -> (string -> unit) -> unit

  val print_functions : (string -> unit) -> unit
  val print_functions_by_category : (string -> unit) -> unit

  val print_functions_md :
    ?extra:bool -> ?deprecated:bool -> (string -> unit) -> unit

  val print_emacs_completions : (string -> unit) -> unit
  val to_json : unit -> Json.t
end

(** Documentation of a plug-in registry, e.g. protocols or decoders. *)
module Plug : sig
  type t = {
    name : string;
    description : string;
    mutable items : (string * string) list;
  }

  val create : doc:string -> string -> t
  val add : t -> doc:string -> string -> unit
  val db : unit -> t list
  val print_md : (string -> unit) -> unit
  val print_string : (string -> unit) -> unit
end

(** Parse a [##] documentation comment. Returns [None] when the comment carries
    no documentation. *)
val parse_doc : pos:Pos.t -> string -> Value.t option
