(** Binding to DSSI SDK.

    @author Samuel Mimram *)

(** Initialize the library. This should be called before any other function. *)
val init : unit -> unit

type event =
  | Event_system of int * int
  | Event_result of int * int
  | Event_note_on of int * int * int
  | Event_note_off of int * int * int

(** Operations on plugins. *)
module Plugin : sig
  (** A plugin. *)
  type t

  (** Error while loading a plugin. *)
  exception Not_a_plugin

  (** Load a plugin. *)
  val load : string -> t

  (** Unload a plugin. *)
  val unload : t -> unit
end

(** Operations on descriptors. *)
module Descriptor : sig
  (** A descriptor. *)
  type t

  exception Not_implemented

  val descriptor : Plugin.t -> int -> t
  val descriptors : Plugin.t -> t array
  val api_version : t -> int
  val ladspa : t -> Ladspa.Descriptor.t
  val configure : t -> Ladspa.Descriptor.instance -> string -> string -> string
  val get_program : t -> Ladspa.Descriptor.instance -> int -> int * int * string
  val select_program : t -> Ladspa.Descriptor.instance -> int -> int -> unit
  val get_midi_controller : t -> Ladspa.Descriptor.instance -> int -> int
  val can_run_synth : t -> bool
  val can_run_synth_adding : t -> bool
  val can_run_multiple_synths : t -> bool
  val can_run_multiple_synths_adding : t -> bool

  val run_multiple_synths :
    t ->
    ?adding:bool ->
    Ladspa.Descriptor.instance array ->
    int ->
    (int * event) array array ->
    unit

  val run_synth :
    t ->
    ?adding:bool ->
    Ladspa.Descriptor.instance ->
    int ->
    (int * event) array ->
    unit
end
