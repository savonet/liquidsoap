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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Values and types of the liquidsoap language. *)

val log : Log.t

(** The type of a value. *)
type t = Lang_types.t

(** {2 Values} *)

(** A typed value. *)
type value = Lang_values.V.value = { mutable t : t; value : in_value }

and full_env = (string * ((int * Lang_types.constraints) list * value)) list

and lazy_full_env =
  (string * ((int * Lang_types.constraints) list * value) Lazy.t) list

and in_value = Lang_values.V.in_value =
  | Bool of bool
  | Int of int
  | String of string
  | Float of float
  | Source of Source.source
  | Request of Request.t
  | Encoder of Encoder.format
  | List of value list
  | Tuple of value list
  | Ref of value ref
  | Fun of
      (string * string * value option) list
      * full_env
      * lazy_full_env
      * Lang_values.term
      (** A function with given arguments (argument label, argument variable,
      default value), parameters already passed to the function, closure and
      value. *)
  | FFI of
      (string * string * value option) list
      * full_env
      * (full_env -> t -> value)

type env = (string * value) list

(** Get a string representation of a value. *)
val print_value : value -> string

(** Iter a function over all sources contained in a value.
  * This only applies to statically referenced objects, ie.
  * it does not explore inside reference cells. *)
val iter_sources : (Source.source -> unit) -> value -> unit

(** {2 Computation} *)

(** Multiapply a value to arguments. The argument [t] is the type of the result
   of the application. *)
val apply : value -> env -> t:t -> value

(** {3 Helpers for registering protocols} *)

val add_protocol :
  syntax:string ->
  doc:string ->
  static:bool ->
  string ->
  Request.resolver ->
  unit

(** {3 Helpers for source builtins} *)

type proto = (string * t * value option * string option) list

(** Some flags that can be attached to operators. *)
type doc_flag =
  | Hidden  (** Don't list the plugin in the documentation. *)
  | Deprecated  (** The plugin should not be used. *)
  | Experimental  (** The plugin should not considered as stable. *)

(** Add an builtin to the language, high-level version for functions. *)
val add_builtin :
  category:string ->
  descr:string ->
  ?flags:doc_flag list ->
  string ->
  proto ->
  t ->
  (env -> t -> value) ->
  unit

(** Add an builtin to the language, more rudimentary version. *)
val add_builtin_base :
  category:string ->
  descr:string ->
  ?flags:doc_flag list ->
  string ->
  in_value ->
  t ->
  unit

(** Category of an operator. *)
type category =
  | Input  (** Input. *)
  | Output  (** Output. *)
  | Conversions  (** Conversions of stream type *)
  | TrackProcessing  (** Operations on tracks (e.g. mixing, etc.). *)
  | SoundProcessing  (** Operations on sound (e.g. compression, etc.). *)
  | VideoProcessing  (** Operations on video. *)
  | MIDIProcessing  (** Operations on MIDI. *)
  | Visualization  (** Visualizations of the sound. *)
  | SoundSynthesis  (** Synthesis. *)
  | Liquidsoap  (** Liquidsoap. *)

(** Get a string representation of a[ category]. *)
val string_of_category : category -> string

(** Get a string representation of a [doc_flag]. *)
val string_of_flag : doc_flag -> string

(** Description of how many channels of given type an operator requires. *)
type lang_kind_format =
  | Fixed of int  (** exactly [n] channels *)
  | Any_fixed of int  (** a fixed number of channels which is at least [n] *)
  | Variable of int
      (** a variable number of channel which always at least [n] *)

(** Description of all the channels an operator requires. *)
type lang_kind_formats =
  | Unconstrained of t  (** no requirements *)
  | Constrained of
      (lang_kind_format, lang_kind_format, lang_kind_format) Frame.fields
      (** specification of requirements for audio, video, etc. *)

val any_fixed : lang_kind_formats

val any_fixed_with :
  ?audio:int -> ?video:int -> ?midi:int -> unit -> lang_kind_formats

val empty : lang_kind_formats
val audio_any : lang_kind_formats
val audio_mono : lang_kind_formats
val audio_stereo : lang_kind_formats
val audio_n : int -> lang_kind_formats
val audio_variable : lang_kind_formats

(** One video channel only. *)
val video_only : lang_kind_formats

(** [n] video channels only. *)
val video_n : int -> lang_kind_formats

(** One video channel, anything for other channels. *)
val video : lang_kind_formats

val midi_n : int -> lang_kind_formats
val midi_only : lang_kind_formats
val audio_video_any : lang_kind_formats
val kind_type_of_kind_format : lang_kind_formats -> t

(** Add an operator to the language and to the documentation. *)
val add_operator :
  category:category ->
  descr:string ->
  ?flags:doc_flag list ->
  ?active:bool ->
  string ->
  proto ->
  kind:lang_kind_formats ->
  (env -> Frame.content_kind -> Source.source) ->
  unit

(** {2 Manipulation of values} *)

val to_unit : value -> unit
val to_bool : value -> bool
val to_bool_getter : value -> unit -> bool
val to_string : value -> string
val to_string_getter : value -> unit -> string
val to_float : value -> float
val to_float_getter : value -> unit -> float
val to_source : value -> Source.source
val to_format : value -> Encoder.format
val to_request : value -> Request.t
val to_int : value -> int
val to_int_getter : value -> unit -> int
val to_list : value -> value list
val to_product : value -> value * value
val to_tuple : value -> value list
val to_metadata_list : value -> (string * string) list
val to_metadata : value -> Frame.metadata
val to_string_list : value -> string list
val to_int_list : value -> int list
val to_source_list : value -> Source.source list
val to_fun : t:t -> value -> (string * value) list -> value

(** [assoc x n l] returns the [n]-th [y] such that [(x,y)] is in the list [l].
  * This is useful for retrieving arguments of a function. *)
val assoc : 'a -> int -> ('a * 'b) list -> 'b

val int_t : t
val unit_t : t
val float_t : t
val bool_t : t
val string_t : t
val product_t : t -> t -> t
val of_product_t : t -> t * t
val tuple_t : t list -> t
val of_tuple_t : t -> t list
val list_t : t -> t
val of_list_t : t -> t
val zero_t : t
val variable_t : t
val succ_t : t -> t
val add_t : int -> t -> t
val type_of_int : int -> t
val request_t : t -> t
val of_request_t : t -> t
val source_t : t -> t
val of_source_t : t -> t
val format_t : t -> t
val frame_kind_t : audio:t -> video:t -> midi:t -> t
val of_frame_kind_t : t -> (t, t, t) Frame.fields
val frame_kind_of_kind_type : t -> Frame.content_kind
val kind_type_of_frame_kind : Frame.content_kind -> t

(** [fun_t args r] is the type of a function taking [args] as parameters
  * and returning values of type [r].
  * The elements of [r] are of the form [(b,l,t)] where [b] indicates if
  * the argument is optional, [l] is the label of the argument ([""] means no
  * label) and [t] is the type of the argument. *)
val fun_t : (bool * string * t) list -> t -> t

val univ_t : ?constraints:Lang_types.constraints -> unit -> t

(** A shortcut for lists of pairs of strings. *)
val metadata_t : t

(** A string getter. The argument is the number of the universal type parameter
  * (should be >= 1). *)
val string_getter_t : unit -> t

(** A float getter. The argument is the number of the universal type parameter
  * (should be >= 1). *)
val float_getter_t : unit -> t

val int_getter_t : unit -> t
val bool_getter_t : unit -> t
val unit : value
val int : int -> value
val bool : bool -> value
val float : float -> value
val string : string -> value
val list : t:t -> value list -> value
val source : Source.source -> value
val request : Request.t -> value
val product : value -> value -> value
val tuple : value list -> value

(** Build a function from an OCaml function.
  * Items in the prototype indicate the label, type and optional
  * values. *)
val val_fun :
  (string * string * t * value option) list ->
  ret_t:t ->
  (env -> t -> value) ->
  value

(** Build a constant function.
  * It is slightly less opaque and allows the printing of the closure
  * when the constant is ground. *)
val val_cst_fun : (string * t * value option) list -> value -> value

(** Convert a metadata packet to a list associating strings to strings. *)
val metadata : Frame.metadata -> value

(** {2 Errors raised by other modules} *)

(** {2 Main script evaluation} *)

(** Load the external libraries. *)
val load_libs : ?parse_only:bool -> unit -> unit

(** Evaluate a script from an [in_channel]. *)
val from_in_channel : ?parse_only:bool -> lib:bool -> in_channel -> unit

(** Evaluate a script from a file. *)
val from_file : ?parse_only:bool -> lib:bool -> string -> unit

(** Evaluate a script from a string. *)
val from_string : ?parse_only:bool -> lib:bool -> string -> unit

(** Interactive loop: read from command line, eval, print and loop. *)
val interactive : unit -> unit

(** Evaluate a string *)
val eval : string -> value option
