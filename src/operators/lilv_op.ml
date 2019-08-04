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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Source
open Lilv

let log = Log.make ["Lilv LV2"]

let conf_lilv =
  Dtools.Conf.void ~p:(Utils.conf#plug "lilv") "Lilv Configuration"

let conf_enable =
  Dtools.Conf.bool ~p:(conf_lilv#plug "enable") ~d:true "Enable LV2 plugins"

class virtual base ~kind source =
object
  inherit operator ~name:"lilv" kind [source]

  method stype = source#stype

  method remaining = source#remaining

  method seek = source#seek

  method is_ready = source#is_ready

  method abort_track = source#abort_track
end

class virtual base_nosource ~kind =
object
  inherit source ~name:"lilv" kind

  method stype = Infallible

  method is_ready = true

  val mutable must_fail = false

  method abort_track =
    must_fail <- true

  method remaining = -1
end

module Data = struct
  let create len = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout len

  let constant len x =
    let data = create len in
    Bigarray.Array1.fill data x;
    data

  let of_array a off len =
  let data = create len in
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set data i a.(i+off)
  done;
  data

  let to_array data a off =
    let len = Bigarray.Array1.dim data in
    for i = 0 to len - 1 do
      a.(i+off) <- Bigarray.Array1.unsafe_get data i
    done
end

(* A plugin is created for each channel. *)
class lilv_mono ~kind (source:source) plugin input output params =
object
  inherit base ~kind source

  val inst = Array.init ((Frame.type_of_kind kind).Frame.audio) (fun _ -> Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate)))

  initializer
    Array.iter Plugin.Instance.activate inst

  method private get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.content buf offset in
    let chans = Array.length b in
    let position = AFrame.position buf in
    let len = position - offset in
    let output_data = Array.init chans (fun _ -> Data.create len) in
    for c = 0 to chans - 1 do
      Plugin.Instance.connect_port_float inst.(c) input (Data.of_array b.(c) offset len);
      Plugin.Instance.connect_port_float inst.(c) output output_data.(c);
      List.iter (fun (p,v) -> Plugin.Instance.connect_port_float inst.(c) p (Data.constant len (v ()))) params;
      Plugin.Instance.run inst.(c) len;
      Data.to_array output_data.(c) b.(c) offset
    done
end

class lilv ~kind (source:source) plugin inputs outputs params =
  let oc = Array.length outputs in
object
  inherit base ~kind source

  val inst = Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate))

  initializer
    Plugin.Instance.activate inst

  method private get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.content buf offset in
    let position = AFrame.position buf in
    let len = position - offset in
    List.iter
      (fun (p,v) ->
        let data = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout len in
        Bigarray.Array1.fill data (v ());
        Plugin.Instance.connect_port_float inst p data)
      params;
    if Array.length inputs = Array.length outputs then
      let chans = Array.length b in
      let output_data = Array.init chans (fun _ -> Data.create len) in
      (* The simple case: number of channels does not get changed. *)
      for c = 0 to chans - 1 do
        Plugin.Instance.connect_port_float inst inputs.(c) (Data.of_array b.(c) offset len);
        Plugin.Instance.connect_port_float inst outputs.(c) output_data.(c)
      done;
      Plugin.Instance.run inst len;
      for c = 0 to chans - 1 do
        Data.to_array output_data.(c) b.(c) offset
      done
    else
      (* We have to change channels. *)
      let d = AFrame.content_of_type ~channels:oc buf offset in
      for c = 0 to Array.length b - 1 do
        Plugin.Instance.connect_port_float inst inputs.(c) (Data.of_array b.(c) offset len)
      done;
      let output_chans = Array.length d in
      let output_data = Array.init output_chans (fun _ -> Data.create len) in
      for c = 0 to output_chans - 1 do
        Plugin.Instance.connect_port_float inst outputs.(c) output_data.(c)
      done;
      Plugin.Instance.run inst len;
      for c = 0 to output_chans - 1 do
        Data.to_array output_data.(c) d.(c) offset
      done
end

class lilv_nosource ~kind plugin outputs params =
object
  inherit base_nosource ~kind

  val inst = Plugin.instantiate plugin (float_of_int (Lazy.force Frame.audio_rate))

  initializer
    Plugin.Instance.activate inst

  method private get_frame buf =
    if must_fail then
      (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false
      )
    else
      let offset = AFrame.position buf in
      let b = AFrame.content buf offset in
      let chans = Array.length b in
      let position = AFrame.size () in
      let len = position - offset in
      let output_data = Array.init chans (fun _ -> Data.create len) in
      List.iter (fun (p,v) -> Plugin.Instance.connect_port_float inst p (Data.constant len (v ()))) params;
      for c = 0 to chans - 1 do
        Plugin.Instance.connect_port_float inst outputs.(c) output_data.(c)
      done;
      Plugin.Instance.run inst len;
      for c = 0 to chans - 1 do
        Data.to_array output_data.(c) b.(c) offset
      done;
      AFrame.add_break buf position
end

(* List the indexes of control ports. *)
let get_control_ports p =
  let ports = Plugin.num_ports p in
  let ans = ref [] in
  for i = 0 to ports - 1 do
    let port = Plugin.port_by_index p i in
    if Port.is_control port && Port.is_input port then
      ans := i :: !ans;
  done;
  List.rev !ans

(* TODO: handle types *)
let port_type _ = `Float

(* Make a parameter for each control port. Returns the liquidsoap parameters
   and the parameters for the plugin. *)
let params_of_plugin plugin =
  let control_ports = get_control_ports plugin in
  let liq_params =
    let univ = ref 0 in
    List.map
      (fun p ->
        let p = Plugin.port_by_index plugin p in
        let t = port_type p in
        incr univ;
        Port.symbol p,
        (match t with
         | `Float -> Lang.float_getter_t !univ
        ),
        (match Port.default_float p with
         | Some f ->
            Some
              (match t with
               | `Float -> Lang.float f
              )
         | None -> None
        ),
        let bounds =
          let min = Port.min_float p in
          let max = Port.max_float p in
          if (min, max) = (None, None) then ""
          else
            let bounds = ref " (" in
            begin match min with
            | Some f ->
               begin match t with
               | `Float -> bounds := Printf.sprintf "%s%.6g <= " !bounds f
               end
            | None -> ()
            end ;
            bounds := !bounds ^ "`" ^ Port.symbol p ^ "`";
            begin match max with
            | Some f ->
               begin match t with
               | `Float -> bounds := Printf.sprintf "%s <= %.6g" !bounds f
               end
            | None -> ()
            end ;
            !bounds ^ ")"
        in
        Some (Port.name p ^ bounds ^ ".")
      )
      control_ports
  in
  let params p =
    let f v = List.assoc v p in
    List.map
      (fun p ->
        p,
        let v = f (Port.symbol (Plugin.port_by_index plugin p)) in
        match port_type p with
        | `Float -> Lang.to_float_getter v
      )
      control_ports
  in
  liq_params, params

(** Get input and output ports. *)
let get_audio_ports p =
  let i = ref [] in
  let o = ref [] in
  let ports = Plugin.num_ports p in
  for n = 0 to ports - 1 do
    let port = Plugin.port_by_index p n in
    if Port.is_audio port then
      if Port.is_input port then
        i := n :: !i
      else
        o := n :: !o
  done;
  Array.of_list (List.rev !i), Array.of_list (List.rev !o)

let register_plugin plugin =
  let inputs, outputs = get_audio_ports plugin in
  let ni = Array.length inputs in
  let no = Array.length outputs in
  let mono = ni = 1 && no = 1 in
  let liq_params, params = params_of_plugin plugin in
  let k = Lang.kind_type_of_kind_format ~fresh:1 (if mono then Lang.any_fixed else Lang.audio_n ni) in
  let liq_params =
    liq_params@(
      if ni = 0 then
        []
      else
        ["", Lang.source_t k, None, None]
    )
  in
  let maker = Plugin.author_name plugin in
  let maker = if maker = "" then "" else " by " ^ maker in
  let descr = Plugin.name plugin ^ maker ^ "." in
  let descr = descr ^ " This is in class " ^ Plugin.Class.label (Plugin.get_class plugin) ^ "." in
  let k =
    if mono then k else
      (* TODO: do we really need a fresh variable here? *)
      Lang.kind_type_of_kind_format ~fresh:1 (Lang.audio_n no)
  in
  Lang.add_operator
    ("lv2." ^ Utils.normalize_parameter_string (Plugin.name plugin))
    liq_params
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~flags:[]
    ~descr
    (fun p kind ->
      let f v = List.assoc v p in
      let source =
        try
          Some (Lang.to_source (f ""))
        with
        | Not_found -> None
      in
      let params = params p in
      if ni = 0 then
        new lilv_nosource ~kind plugin outputs params
      else if mono then
        new lilv_mono ~kind (Utils.get_some source) plugin inputs.(0) outputs.(0) params
      else
        new lilv ~kind (Utils.get_some source) plugin inputs outputs params
    )

let register_plugins () =
  let world = World.create () in
  World.load_all world;
  Plugins.iter register_plugin (World.plugins world)

let () =
  Configure.at_init (fun () -> if conf_enable#get then register_plugins ())
