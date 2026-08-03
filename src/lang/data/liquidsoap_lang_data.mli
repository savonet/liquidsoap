(** Self-contained services the rest of the language builds on. *)

(** Errors raised while a script runs. *)
module Runtime_error = Runtime_error

(** How a value was written in the source, e.g. hexadecimal or octal. *)
module Flags = Flags

(** Immutable maps of methods, hashed for the cache. *)
module Methods = Methods

(** JSON parsing and rendering. *)
module Json = Json

(** Documentation of builtins, operators and plug-in registries. *)
module Doc = Doc

(** Registries that plug-ins register into, e.g. protocols and decoders. *)
module Plug = Plug

(** Path resolution and a couple of formatting helpers. *)
module Utils = Utils

(** Messages emitted before the logger is up. *)
module Startup = Startup

(** Timing of script evaluation. *)
module Profiler = Profiler

(** Version and build-time configuration. *)
module Build_config = Build_config

(** Installed paths, from dune-site. *)
module Sites = Sites
