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

open Mm
open Source
open Ladspa
module Cache = Liquidsoap_lang.Cache

module String = struct
  include String

  (** Division for strings. *)
  let residual s t =
    let m = String.length s in
    let n = String.length t in
    if m >= n && String.sub s 0 n = t then String.sub s n (m - n)
    else raise Not_found
end

let ladspa = Lang.add_module "ladspa"

type t = Float | Int | Bool

let log = Log.make ["LADSPA extension"]

let ladspa_enabled =
  try
    let venv = Unix.getenv "LIQ_LADSPA" in
    venv = "1" || venv = "true"
  with Not_found -> true

let ladspa_dirs =
  try String.split_on_char ':' (Unix.getenv "LIQ_LADSPA_DIRS")
  with Not_found ->
    ["/usr/lib64/ladspa"; "/usr/lib/ladspa"; "/usr/local/lib/ladspa"]

let port_t d p =
  if Descriptor.port_is_boolean d p then Bool
  else if Descriptor.port_is_integer d p then Int
  else Float

class virtual base source =
  object
    inherit operator ~name:"ladspa" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method self_sync = source#self_sync
    method abort_track = source#abort_track
  end

class virtual base_nosource =
  object (self)
    inherit source ~name:"ladspa" ()
    method effective_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    method self_sync = (`Static, None)
    val mutable must_fail = false
    method abort_track = must_fail <- true
    method remaining = -1
  end

let instantiate d samplerate =
  let ans = Descriptor.instantiate d samplerate in
  (* Connect output control ports (which we don't use) to some dummy buffer in
     order to avoid segfaults. *)
  for i = 0 to Descriptor.port_count d - 1 do
    if Descriptor.port_is_control d i && not (Descriptor.port_is_input d i) then (
      let c = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
      Descriptor.connect_port ans i c)
  done;
  ans

(* A plugin is created for each channel. *)
class ladspa_mono (source : source) plugin descr input output params =
  object (self)
    inherit base source
    val mutable inst = None

    initializer
      self#on_wake_up (fun () ->
          let p = Plugin.load plugin in
          let d = Descriptor.descriptor p descr in
          let i =
            Array.init
              (Content.Audio.channels_of_format
                 (Option.get
                    (Frame.Fields.find_opt Frame.Fields.audio self#content_type)))
              (fun _ -> instantiate d (Lazy.force Frame.audio_rate))
          in
          Array.iter Descriptor.activate i;
          inst <- Some i)

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let len = source#frame_audio_position in
      let inst = Option.get inst in
      for c = 0 to Array.length b - 1 do
        let buf = Audio.Mono.to_ba b.(c) 0 len in
        Descriptor.connect_port inst.(c) input buf;
        Descriptor.connect_port inst.(c) output buf;
        List.iter
          (fun (p, v) -> Descriptor.set_control_port inst.(c) p (v ()))
          params;
        Descriptor.run inst.(c) len;
        Audio.Mono.copy_from_ba buf b.(c) 0 len
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

class ladspa (source : source) plugin descr inputs outputs params =
  object
    inherit base source

    val inst =
      let p = Plugin.load plugin in
      let d = Descriptor.descriptor p descr in
      instantiate d (Lazy.force Frame.audio_rate)

    initializer Descriptor.activate inst

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let len = source#frame_audio_position in
      let ba = Audio.to_ba b 0 len in
      List.iter (fun (p, v) -> Descriptor.set_control_port inst p (v ())) params;
      if Array.length inputs = Array.length outputs then (
        (* The simple case: number of channels does not get changed. *)
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst inputs.(c) ba.(c);
          Descriptor.connect_port inst outputs.(c) ba.(c)
        done;
        Descriptor.run inst len;
        Audio.copy_from_ba ba b 0 len)
      else (
        (* We have to change channels. *)
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst inputs.(c) ba.(c)
        done;
        let dba = Audio.to_ba b 0 len in
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst outputs.(c) dba.(c)
        done;
        Descriptor.run inst len;
        Audio.copy_from_ba dba b 0 len);
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

class ladspa_nosource plugin descr outputs params =
  object (self)
    inherit base_nosource

    val inst =
      let p = Plugin.load plugin in
      let d = Descriptor.descriptor p descr in
      instantiate d (Lazy.force Frame.audio_rate)

    initializer Descriptor.activate inst

    method private generate_frame =
      if must_fail then (
        must_fail <- false;
        self#end_of_track)
      else (
        let length = Lazy.force Frame.size in
        let buf = Frame.create ~length self#content_type in
        let b = Content.Audio.get_data (Frame.get buf Frame.Fields.audio) in
        List.iter
          (fun (p, v) -> Descriptor.set_control_port inst p (v ()))
          params;
        let alen = Frame.audio_of_main length in
        let ba = Audio.to_ba b 0 alen in
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst outputs.(c) ba.(c)
        done;
        Descriptor.run inst alen;
        Audio.copy_from_ba ba b 0 alen;
        Frame.set_data buf Frame.Fields.audio Content.Audio.lift_data b)
  end

(* List the indexes of control ports. *)
let get_control_ports d =
  let ports = Descriptor.port_count d in
  let ans = ref [] in
  for i = 0 to ports - 1 do
    if Descriptor.port_is_control d i && Descriptor.port_is_input d i then
      ans := i :: !ans
  done;
  List.rev !ans

type port = {
  port_index : int;
  port_name : string;
  port_type : t;
  port_default : float option;
  port_min : float option;
  port_max : float option;
}

(** When creating operator for LADSPA plugins, we don't know yet at which
    samplerate Liquidsoap will operate. But the default values and bounds for
    LADSPA parameters might depend on the samplerate. Lacking a better solution,
    we use the following default samplerate, potentially creating a mismatch
    between the doc and the actual behavior. *)
let default_samplerate = 44100

let get_control_ports d =
  List.map
    (fun p ->
      {
        port_index = p;
        port_name = Utils.normalize_parameter_string (Descriptor.port_name d p);
        port_type = port_t d p;
        port_default =
          Descriptor.port_get_default d ~samplerate:default_samplerate p;
        port_min = Descriptor.port_get_min d ~samplerate:default_samplerate p;
        port_max = Descriptor.port_get_max d ~samplerate:default_samplerate p;
      })
    (get_control_ports d)

type plugin = {
  plugin_file : string;
  plugin_descriptor : int;
  plugin_inputs : int array;
  plugin_outputs : int array;
  plugin_controls : port list;
  plugin_name : string;
  plugin_label : string;
  plugin_maker : string;
}

(** Get input and output ports. *)
let get_audio_ports d =
  let i = ref [] in
  let o = ref [] in
  let ports = Descriptor.port_count d in
  for n = 0 to ports - 1 do
    if Descriptor.port_is_audio d n then
      if Descriptor.port_is_input d n then i := n :: !i else o := n :: !o
  done;
  (Array.of_list (List.rev !i), Array.of_list (List.rev !o))

let load_descriptor fname descr d =
  let plugin_inputs, plugin_outputs = get_audio_ports d in
  {
    plugin_file = fname;
    plugin_descriptor = descr;
    plugin_inputs;
    plugin_outputs;
    plugin_controls = get_control_ports d;
    plugin_name = Descriptor.name d;
    plugin_label = Utils.normalize_parameter_string (Descriptor.label d);
    plugin_maker = Descriptor.maker d;
  }

(* Make a parameter for each control port. Returns the liquidsoap parameters and the parameters for the plugin. *)
let params_of_controls control_ports =
  let liq_params =
    List.map
      (fun p ->
        let t = p.port_type in
        ( p.port_name,
          (match t with
            | Float -> Lang.getter_t Lang.float_t
            | Int -> Lang.getter_t Lang.int_t
            | Bool -> Lang.getter_t Lang.bool_t),
          (match p.port_default with
            | Some f ->
                Some
                  (match t with
                    | Float -> Lang.float f
                    | Int -> Lang.int (int_of_float f)
                    | Bool -> Lang.bool (f > 0.))
            | None -> None),
          let bounds =
            let min = p.port_min in
            let max = p.port_max in
            if (min, max) = (None, None) then ""
            else (
              let bounds = ref " (" in
              begin match min with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s%.6g <= " !bounds f
                      | Int ->
                          bounds :=
                            Printf.sprintf "%s%d <= " !bounds
                              (int_of_float (ceil f))
                      | Bool -> ())
                | None -> ()
              end;
              bounds := !bounds ^ "`" ^ p.port_name ^ "`";
              begin match max with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s <= %.6g" !bounds f
                      | Int ->
                          bounds :=
                            Printf.sprintf "%s <= %d" !bounds (int_of_float f)
                      | Bool -> ())
                | None -> ()
              end;
              !bounds ^ ")")
          in
          Some (p.port_name ^ bounds ^ ".") ))
      control_ports
  in
  let params l =
    List.map
      (fun p ->
        ( p.port_index,
          let v = List.assoc p.port_name l in
          match p.port_type with
            | Float -> Lang.to_float_getter v
            | Int ->
                let f = Lang.to_int_getter v in
                fun () -> float_of_int (f ())
            | Bool ->
                let f = Lang.to_bool_getter v in
                fun () -> if f () then 1. else 0. ))
      control_ports
  in
  (liq_params, params)

let register_descr d =
  let ni = Array.length d.plugin_inputs in
  let no = Array.length d.plugin_outputs in
  let mono = ni = 1 && no = 1 in
  let liq_params, params = params_of_controls d.plugin_controls in
  let input_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  let liq_params =
    liq_params
    @ if ni = 0 then [] else [("", Lang.source_t input_t, None, None)]
  in
  let maker = d.plugin_maker in
  let maker = String.concat "(at)" (String.split_on_char '@' maker) in
  let descr = Printf.sprintf "%s by %s." d.plugin_name maker in
  let return_t =
    if mono then input_t
    else
      Frame_type.set_field input_t Frame.Fields.audio (Format_type.audio_n no)
  in
  let label = d.plugin_label in
  let label =
    try "lsp_" ^ String.residual label "http:_lsp_plugin_plugins_ladspa_"
    with Not_found -> label
  in
  ignore
    (Lang.add_operator ~base:ladspa label liq_params ~return_t ~category:`Audio
       ~flags:[`Extra] ~descr (fun p ->
         let f v = List.assoc v p in
         let source =
           try Some (Lang.to_source (f "")) with Not_found -> None
         in
         let params = params p in
         if ni = 0 then
           new ladspa_nosource
             d.plugin_file d.plugin_descriptor d.plugin_outputs params
         else if mono then
           (new ladspa_mono
              (Option.get source) d.plugin_file d.plugin_descriptor
              d.plugin_inputs.(0) d.plugin_outputs.(0) params
             :> Source.source)
         else
           (new ladspa
              (Option.get source) d.plugin_file d.plugin_descriptor
              d.plugin_inputs d.plugin_outputs params
             :> Source.source)))

let register_descr d =
  (* We do not register plugins without outputs for now. *)
  try
    ignore
      (Audio_converter.Channel_layout.layout_of_channels
         (Array.length d.plugin_inputs));
    ignore
      (Audio_converter.Channel_layout.layout_of_channels
         (Array.length d.plugin_outputs));
    if d.plugin_outputs <> [||] then register_descr d
  with Audio_converter.Channel_layout.Unsupported ->
    log#info
      "Could not register LADSPA plugin %s: unhandled number of channels."
      d.plugin_file

let register_plugin cache pname =
  try
    let p = Plugin.load pname in
    let descr = Descriptor.descriptors p in
    Array.iteri
      (fun n d ->
        let key = pname ^ string_of_int n in
        let d =
          Cache.Table.get cache key (fun () -> load_descriptor pname n d)
        in
        register_descr d)
      descr;
    Plugin.unload p
  with Plugin.Not_a_plugin -> ()

let register_plugins () =
  let cache =
    (Cache.Table.load ~dirtype:`System ~name:"LADSPA plugins" "ladspa-plugins"
      : plugin Cache.Table.t)
  in
  let add plugins_dir =
    try
      let dir = Unix.opendir plugins_dir in
      try
        while true do
          let f = Unix.readdir dir in
          if f <> "." && f <> ".." then
            register_plugin cache (plugins_dir ^ "/" ^ f)
        done
      with End_of_file -> Unix.closedir dir
    with Unix.Unix_error (e, _, _) ->
      log#info "Error while loading directory %s: %s" plugins_dir
        (Unix.error_message e)
  in
  List.iter add ladspa_dirs;
  Cache.Table.store ~dirtype:`System cache

let () =
  Lifecycle.on_load ~name:"ladspa plugin registration" (fun () ->
      if !Startup.register_external_plugins && ladspa_enabled then
        Startup.time "LADSPA plugins registration" register_plugins)
