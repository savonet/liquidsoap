(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
open Lilv
module Cache = Liquidsoap_lang.Cache

let lv2 = Lang.add_module "lv2"
let log = Log.make ["Lilv LV2"]

let lilv_enabled =
  try
    let venv = Unix.getenv "LIQ_LILV" in
    venv = "1" || venv = "true"
  with Not_found -> true

class virtual base source =
  object
    inherit operator ~name:"lilv" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method self_sync = source#self_sync
    method abort_track = source#abort_track
  end

class virtual base_nosource =
  object (self)
    inherit source ~name:"lilv" ()
    method effective_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    val mutable must_fail = false
    method abort_track = must_fail <- true
    method remaining = -1
  end

let constant_data len x =
  let data = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout len in
  Bigarray.Array1.fill data x;
  data

(** A mono LV2 plugin: a plugin is created for each channel. *)
class lilv_mono (source : source) plugin input output params =
  object (self)
    inherit base source
    val mutable inst = None

    initializer
      self#on_wake_up (fun () ->
          let i =
            Array.init
              (Content.Audio.channels_of_format
                 (Option.get
                    (Frame.Fields.find_opt Frame.Fields.audio self#content_type)))
              (fun _ ->
                Plugin.instantiate plugin
                  (float_of_int (Lazy.force Frame.audio_rate)))
          in
          Array.iter Plugin.Instance.activate i;
          inst <- Some i)

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let len = source#frame_audio_position in
      let chans = Array.length b in
      let inst = Option.get inst in
      let ba = Audio.to_ba b 0 len in
      for c = 0 to chans - 1 do
        Plugin.Instance.connect_port_float inst.(c) input ba.(c);
        Plugin.Instance.connect_port_float inst.(c) output ba.(c);
        List.iter
          (fun (p, v) ->
            Plugin.Instance.connect_port_float inst.(c) p
              (constant_data len (v ())))
          params;
        Plugin.Instance.run inst.(c) len;
        Audio.copy_from_ba ba b 0 len
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

class lilv (source : source) plugin inputs outputs params =
  object
    inherit base source

    val inst =
      Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate))

    initializer Plugin.Instance.activate inst

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let len = source#frame_audio_position in
      List.iter
        (fun (p, v) ->
          let data =
            Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout len
          in
          Bigarray.Array1.fill data (v ());
          Plugin.Instance.connect_port_float inst p data)
        params;
      let ba = Audio.to_ba b 0 len in
      if Array.length inputs = Array.length outputs then (
        let chans = Array.length b in
        (* The simple case: number of channels does not get changed. *)
        for c = 0 to chans - 1 do
          Plugin.Instance.connect_port_float inst inputs.(c) ba.(c);
          Plugin.Instance.connect_port_float inst outputs.(c) ba.(c)
        done;
        Plugin.Instance.run inst len;
        Audio.copy_from_ba ba b 0 len)
      else (
        (* We have to change channels. *)
        let dba = Audio.to_ba b 0 len in
        for c = 0 to Array.length b - 1 do
          Plugin.Instance.connect_port_float inst inputs.(c) ba.(c)
        done;
        let output_chans = Array.length b in
        for c = 0 to output_chans - 1 do
          Plugin.Instance.connect_port_float inst outputs.(c) dba.(c)
        done;
        Plugin.Instance.run inst len;
        Audio.copy_from_ba dba b 0 len);
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

(** An LV2 plugin without audio input. *)
class lilv_nosource plugin outputs params =
  object (self)
    inherit base_nosource
    method self_sync = (`Static, None)

    val inst =
      Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate))

    initializer Plugin.Instance.activate inst

    method private generate_frame =
      if must_fail then (
        must_fail <- false;
        self#end_of_track)
      else (
        let length = Lazy.force Frame.size in
        let buf = Frame.create ~length self#content_type in
        let b = Content.Audio.get_data (Frame.get buf Frame.Fields.audio) in
        let chans = Array.length b in
        let alen = Frame.audio_of_main length in
        let ba = Audio.to_ba b 0 alen in
        List.iter
          (fun (p, v) ->
            Plugin.Instance.connect_port_float inst p
              (constant_data alen (v ())))
          params;
        for c = 0 to chans - 1 do
          Plugin.Instance.connect_port_float inst outputs.(c) ba.(c)
        done;
        Plugin.Instance.run inst alen;
        Audio.copy_from_ba ba b 0 alen;
        Frame.set_data buf Frame.Fields.audio Content.Audio.lift_data b)
  end

(** An LV2 plugin without audio output (e.g. to observe the stream). The input
    stream is returned. *)
class lilv_noout source plugin inputs params =
  object
    inherit base source

    val inst =
      Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate))

    initializer Plugin.Instance.activate inst

    method private generate_frame =
      let buf = source#get_frame in
      let b = Content.Audio.get_data (Frame.get buf Frame.Fields.audio) in
      let chans = Array.length b in
      let alen = source#frame_audio_position in
      let ba = Audio.to_ba b 0 alen in
      List.iter
        (fun (p, v) ->
          Plugin.Instance.connect_port_float inst p (constant_data alen (v ())))
        params;
      for c = 0 to chans - 1 do
        Plugin.Instance.connect_port_float inst inputs.(c) ba.(c)
      done;
      Plugin.Instance.run inst alen;
      buf
  end

(* List the indexes of control ports. *)
let get_control_ports p =
  let ports = Plugin.num_ports p in
  let ans = ref [] in
  for i = 0 to ports - 1 do
    let port = Plugin.port_by_index p i in
    if Port.is_control port && Port.is_input port then ans := i :: !ans
  done;
  List.rev !ans

type port_type = Float

(* TODO: handle types *)
let port_type _ = Float

type port = {
  port_index : int;
  port_symbol : string;
  port_name : string;
  port_type : port_type;
  port_default_float : float option;
  port_min_float : float option;
  port_max_float : float option;
}

let get_control_ports plugin =
  let control_ports = get_control_ports plugin in
  (* TODO: handle other types *)
  let control_ports =
    List.filter (fun p -> port_type p = Float) control_ports
  in
  List.map
    (fun i ->
      let p = Plugin.port_by_index plugin i in
      {
        port_index = i;
        port_symbol = Port.symbol p;
        port_name = Port.name p;
        port_type = port_type p;
        port_default_float = Port.default_float p;
        port_min_float = Port.min_float p;
        port_max_float = Port.max_float p;
      })
    control_ports

(** Get input and output ports. *)
let get_audio_ports p =
  let i = ref [] in
  let o = ref [] in
  let ports = Plugin.num_ports p in
  for n = 0 to ports - 1 do
    let port = Plugin.port_by_index p n in
    if Port.is_audio port then
      if Port.is_input port then i := n :: !i else o := n :: !o
  done;
  (Array.of_list (List.rev !i), Array.of_list (List.rev !o))

type plugin = {
  plugin_uri : string;
  plugin_name : string;
  plugin_inputs : int array;
  plugin_outputs : int array;
  plugin_controls : port list;  (** control ports *)
  plugin_maker : string;
  plugin_class_label : string;
}

let load_plugin plugin =
  let plugin_inputs, plugin_outputs = get_audio_ports plugin in
  let plugin_controls = get_control_ports plugin in
  let maker = Plugin.author_name plugin in
  let maker_homepage = Plugin.author_homepage plugin in
  let maker =
    if maker_homepage = "" then maker
    else Printf.sprintf "[%s](%s)" maker maker_homepage
  in
  let plugin_maker = if maker = "" then "" else " by " ^ maker in
  let plugin_class_label = Plugin.Class.label (Plugin.plugin_class plugin) in
  {
    plugin_uri = Plugin.uri plugin;
    plugin_name = Plugin.name plugin;
    plugin_inputs;
    plugin_outputs;
    plugin_controls;
    plugin_maker;
    plugin_class_label;
  }

(* Make a parameter for each control port. Returns the liquidsoap parameters
   and the parameters for the plugin. *)
let params_of_controls control_ports =
  let liq_params =
    List.map
      (fun p ->
        let t = p.port_type in
        ( p.port_symbol,
          (match t with Float -> Lang.getter_t Lang.float_t),
          (match p.port_default_float with
            | Some f -> Some (match t with Float -> Lang.float f)
            | None -> None),
          let bounds =
            let min = p.port_min_float in
            let max = p.port_max_float in
            if (min, max) = (None, None) then ""
            else (
              let bounds = ref " (" in
              begin match min with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s%.6g <= " !bounds f
                    )
                | None -> ()
              end;
              bounds := !bounds ^ "`" ^ p.port_symbol ^ "`";
              begin match max with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s <= %.6g" !bounds f
                    )
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
          let v = List.assoc p.port_symbol l in
          match port_type p with Float -> Lang.to_float_getter v ))
      control_ports
  in
  (liq_params, params)

let register_plugin plugin p =
  let ni = Array.length p.plugin_inputs in
  let no = Array.length p.plugin_outputs in
  (* Ensure that we support the number of channels. *)
  ignore (Audio_converter.Channel_layout.layout_of_channels ni);
  ignore (Audio_converter.Channel_layout.layout_of_channels no);
  let mono = ni = 1 && no = 1 in
  let input_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio_n ni) ())
  in
  let liq_params, params = params_of_controls p.plugin_controls in
  let liq_params =
    liq_params
    @ if ni = 0 then [] else [("", Lang.source_t input_t, None, None)]
  in

  let descr = p.plugin_name ^ p.plugin_maker ^ "." in
  let descr = descr ^ " This is in class " ^ p.plugin_class_label ^ "." in
  let descr = descr ^ " See <" ^ p.plugin_uri ^ ">." in
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio_n no) ())
  in
  ignore
    (Lang.add_operator ~base:lv2
       (Utils.normalize_parameter_string p.plugin_name)
       liq_params ~return_t ~category:`Audio ~flags:[`Extra] ~descr (fun l ->
         let f v = List.assoc v l in
         let source =
           try Some (Lang.to_source (f "")) with Not_found -> None
         in
         let params = params l in
         if ni = 0 then new lilv_nosource plugin p.plugin_outputs params
         else if no = 0 then
           (* TODO: can we really use such a type? *)
           (new lilv_noout (Option.get source) plugin p.plugin_inputs params
             :> Source.source)
         else if mono then
           (new lilv_mono
              (Option.get source) plugin p.plugin_inputs.(0)
              p.plugin_outputs.(0) params
             :> Source.source)
         else
           (new lilv
              (Option.get source) plugin p.plugin_inputs p.plugin_outputs params
             :> Source.source)))

let register_plugin cache plugin =
  (* Only the uri computation is fast. Try to retrieve other parameters from the cache. *)
  let uri = Plugin.uri plugin in
  let p = Cache.Table.get cache uri (fun () -> load_plugin plugin) in
  try register_plugin plugin p
  with Audio_converter.Channel_layout.Unsupported ->
    log#info "Could not register Lilv plugin %s: unhandled number of channels."
      p.plugin_name

let register_plugins () =
  let cache =
    (Cache.Table.load ~dirtype:`System ~name:"lilv plugins" "lilv-plugins"
      : plugin Cache.Table.t)
  in
  let world = World.create () in
  World.load_all world;
  Plugins.iter (register_plugin cache) (World.plugins world);
  Cache.Table.store ~dirtype:`System cache

let () =
  Lifecycle.on_load ~name:"lilv plugin registration" (fun () ->
      if !Startup.register_external_plugins && lilv_enabled then
        Startup.time "Lilv plugins registration" register_plugins)
