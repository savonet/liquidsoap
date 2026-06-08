(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of ocaml-dssi.
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type event =
  | Event_system of int * int
  | Event_result of int * int
  (* Event_note is forbidden by DSSI specification. *)
  | Event_note_on of int * int * int
  | Event_note_off of int * int * int

module Plugin = struct
  type t

  exception Not_a_plugin

  external load : string -> t = "ocaml_dssi_open"
  external unload : t -> unit = "ocaml_dssi_close"
end

module Descriptor = struct
  type t

  exception Not_implemented

  external descriptor : Plugin.t -> int -> t = "ocaml_dssi_descriptor"

  let rec descriptors p n =
    try
      let d = descriptor p n in
      let dd = descriptors p (n + 1) in
      d :: dd
    with Not_found -> []

  let descriptors p = Array.of_list (descriptors p 0)

  external api_version : t -> int = "ocaml_dssi_api_version"
  external ladspa : t -> Ladspa.Descriptor.t = "ocaml_dssi_ladspa"

  external configure :
    t -> Ladspa.Descriptor.instance -> string -> string -> string
    = "ocaml_dssi_configure"

  external get_program :
    t -> Ladspa.Descriptor.instance -> int -> int * int * string
    = "ocaml_dssi_get_program"

  external select_program :
    t -> Ladspa.Descriptor.instance -> int -> int -> unit
    = "ocaml_dssi_select_program"

  external get_midi_controller : t -> Ladspa.Descriptor.instance -> int -> int
    = "ocaml_dssi_get_midi_controller_for_port"

  external can_run_synth : t -> bool = "ocaml_dssi_can_run_synth"
  external can_run_synth_adding : t -> bool = "ocaml_dssi_can_run_synth_adding"

  external run_synth :
    t ->
    bool ->
    Ladspa.Descriptor.instance ->
    int ->
    (int * event) array ->
    unit = "ocaml_dssi_run_synth"

  external can_run_multiple_synths : t -> bool
    = "ocaml_dssi_can_run_multiple_synths"

  external can_run_multiple_synths_adding : t -> bool
    = "ocaml_dssi_can_run_multiple_synths_adding"

  external run_multiple_synths :
    t ->
    bool ->
    Ladspa.Descriptor.instance array ->
    int ->
    (int * event) array array ->
    unit = "ocaml_dssi_run_multiple_synths"

  let run_multiple_synths d ?(adding = false) i n e =
    if adding then (
      try run_multiple_synths d true i n e
      with Not_implemented -> run_multiple_synths d false i n e)
    else run_multiple_synths d false i n e

  let run_synth d ?(adding = false) i n e =
    try
      try run_synth d adding i n e
      with Not_implemented ->
        if adding then run_synth d false i n e else raise Not_implemented
    with Not_implemented -> run_multiple_synths d ~adding [| i |] n [| e |]
end

let init () =
  Callback.register_exception "ocaml_dssi_exn_not_found" Not_found;
  Callback.register_exception "ocaml_dssi_exn_not_a_plugin" Plugin.Not_a_plugin;
  Callback.register_exception "ocaml_dssi_exn_not_implemented"
    Descriptor.Not_implemented

let () = init ()
