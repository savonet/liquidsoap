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
open Ladspa

let () = Lang.add_module "ladspa"

type t = Float | Int | Bool

let log = Log.make ["LADSPA extension"]

let conf_ladspa =
  Dtools.Conf.void ~p:(Utils.conf#plug "ladspa") "Lasdpa Configuration"

let conf_enable =
  Dtools.Conf.bool ~p:(conf_ladspa#plug "enable") ~d:true "Enable Ladspa "

let conf_dirs =
  Dtools.Conf.list ~p:(conf_ladspa#plug "dirs")
    ~d:["/usr/lib64/ladspa"; "/usr/lib/ladspa"; "/usr/local/lib/ladspa"]
    "Directories to search for plugins"

let port_t d p =
  if Descriptor.port_is_boolean d p then Bool
  else if Descriptor.port_is_integer d p then Int
  else Float

class virtual base ~kind source =
  object
    inherit operator ~name:"ladspa" kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method self_sync = source#self_sync

    method abort_track = source#abort_track
  end

class virtual base_nosource ~kind =
  object
    inherit source ~name:"ladspa" kind

    method stype = Infallible

    method is_ready = true

    method self_sync = false

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
      Descriptor.connect_port ans i c )
  done;
  ans

(* A plugin is created for each channel. *)
class ladspa_mono ~kind (source : source) plugin descr input output params =
  object (self)
    inherit base ~kind source as super

    val mutable inst = None

    method wake_up a =
      super#wake_up a;
      let p = Plugin.load plugin in
      let d = Descriptor.descriptor p descr in
      let i =
        Array.init self#ctype.Frame.audio (fun _ ->
            instantiate d (Lazy.force Frame.audio_rate))
      in
      Array.iter Descriptor.activate i;
      inst <- Some i

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf in
      let position = AFrame.position buf in
      let len = position - offset in
      let inst = Option.get inst in
      for c = 0 to Array.length b - 1 do
        let buf = Audio.Mono.sub b.(c) offset len in
        Descriptor.connect_port inst.(c) input buf;
        Descriptor.connect_port inst.(c) output buf;
        List.iter
          (fun (p, v) -> Descriptor.set_control_port inst.(c) p (v ()))
          params;
        Descriptor.run inst.(c) len
      done
  end

class ladspa ~kind (source : source) plugin descr inputs outputs params =
  object (self)
    inherit base ~kind source

    val inst =
      let p = Plugin.load plugin in
      let d = Descriptor.descriptor p descr in
      instantiate d (Lazy.force Frame.audio_rate)

    initializer Descriptor.activate inst

    method set_kind =
      Source.Kind.unify self#kind_var
        (Source.Kind.set_audio source#kind_var (Array.length outputs))

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf in
      let position = AFrame.position buf in
      let len = position - offset in
      List.iter (fun (p, v) -> Descriptor.set_control_port inst p (v ())) params;
      if Array.length inputs = Array.length outputs then (
        (* The simple case: number of channels does not get changed. *)
        for c = 0 to Array.length b - 1 do
          let buf = Audio.Mono.sub b.(c) offset len in
          Descriptor.connect_port inst inputs.(c) buf;
          Descriptor.connect_port inst outputs.(c) buf
        done;
        Descriptor.run inst len )
      else (
        (* We have to change channels. *)
        let d = AFrame.content buf in
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst inputs.(c)
            (Audio.Mono.sub b.(c) offset len)
        done;
        for c = 0 to Array.length d - 1 do
          Descriptor.connect_port inst outputs.(c)
            (Audio.Mono.sub d.(c) offset len)
        done;
        Descriptor.run inst len )
  end

class ladspa_nosource ~kind plugin descr outputs params =
  object
    inherit base_nosource ~kind

    val inst =
      let p = Plugin.load plugin in
      let d = Descriptor.descriptor p descr in
      instantiate d (Lazy.force Frame.audio_rate)

    initializer Descriptor.activate inst

    method private get_frame buf =
      if must_fail then (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false )
      else (
        let offset = AFrame.position buf in
        let b = AFrame.content buf in
        let position = AFrame.size () in
        let len = position - offset in
        List.iter
          (fun (p, v) -> Descriptor.set_control_port inst p (v ()))
          params;
        for c = 0 to Array.length b - 1 do
          Descriptor.connect_port inst outputs.(c)
            (Audio.Mono.sub b.(c) offset len)
        done;
        Descriptor.run inst len;
        AFrame.add_break buf position )
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

(** When creating operator for LADSPA plugins, we don't know yet at which
    samplerate Liquidsoap will operate. But the default values and bounds for
    LADSPA parameters might depend on the samplerate. Lacking a better solution,
    we use the following default samplerate, potentially creating a mismatch
    between the doc and the actual behavior. *)
let default_samplerate = 44100

(* Make a parameter for each control port.
 * Returns the liquidsoap parameters and the parameters for the plugin. *)
let params_of_descr d =
  let control_ports = get_control_ports d in
  let liq_params =
    List.map
      (fun p ->
        let t = port_t d p in
        ( Utils.normalize_parameter_string (Descriptor.port_name d p),
          ( match t with
            | Float -> Lang.float_getter_t ()
            | Int -> Lang.int_getter_t ()
            | Bool -> Lang.bool_getter_t () ),
          ( match
              Descriptor.port_get_default d ~samplerate:default_samplerate p
            with
            | Some f ->
                Some
                  ( match t with
                    | Float -> Lang.float f
                    | Int -> Lang.int (int_of_float f)
                    | Bool -> Lang.bool (f > 0.) )
            | None -> None ),
          let bounds =
            let min =
              Descriptor.port_get_min d ~samplerate:default_samplerate p
            in
            let max =
              Descriptor.port_get_max d ~samplerate:default_samplerate p
            in
            if (min, max) = (None, None) then ""
            else (
              let bounds = ref " (" in
              begin
                match min with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s%.6g <= " !bounds f
                      | Int ->
                          bounds :=
                            Printf.sprintf "%s%d <= " !bounds
                              (int_of_float (ceil f))
                      | Bool -> () )
                | None -> ()
              end;
              bounds :=
                !bounds ^ "`"
                ^ Utils.normalize_parameter_string (Descriptor.port_name d p)
                ^ "`";
              begin
                match max with
                | Some f -> (
                    match t with
                      | Float -> bounds := Printf.sprintf "%s <= %.6g" !bounds f
                      | Int ->
                          bounds :=
                            Printf.sprintf "%s <= %d" !bounds (int_of_float f)
                      | Bool -> () )
                | None -> ()
              end;
              !bounds ^ ")" )
          in
          Some (Descriptor.port_name d p ^ bounds ^ ".") ))
      control_ports
  in
  let params p =
    let f v = List.assoc v p in
    List.map
      (fun p ->
        ( p,
          let v =
            f (Utils.normalize_parameter_string (Descriptor.port_name d p))
          in
          match port_t d p with
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

let register_descr plugin_name descr_n d inputs outputs =
  let ni = Array.length inputs in
  let no = Array.length outputs in
  let mono = ni = 1 && no = 1 in
  let liq_params, params = params_of_descr d in
  let input_kind =
    if mono then Lang.any else { Lang.any with Frame.audio = Lang.Fixed ni }
  in
  let input_t = Lang.kind_type_of_kind_format input_kind in
  let liq_params =
    liq_params
    @ if ni = 0 then [] else [("", Lang.source_t input_t, None, None)]
  in
  let maker = Descriptor.maker d in
  let maker = Pcre.substitute ~pat:"@" ~subst:(fun _ -> "(at)") maker in
  let descr = Printf.sprintf "%s by %s." (Descriptor.name d) maker in
  let return_t =
    if mono then input_t
    else if ni = 0 then Lang.kind_type_of_kind_format (Lang.audio_n no)
    else (
      let { Frame.video; midi } = Lang.of_frame_kind_t input_t in
      Lang.frame_kind_t ~audio:(Lang.n_t no) ~video ~midi )
  in
  Lang.add_operator
    ("ladspa." ^ Utils.normalize_parameter_string (Descriptor.label d))
    liq_params ~return_t ~category:Lang.SoundProcessing ~flags:[] ~descr
    (fun p ->
      let f v = List.assoc v p in
      let source = try Some (Lang.to_source (f "")) with Not_found -> None in
      let params = params p in
      if ni = 0 then (
        let kind = Lang.audio_n no in
        new ladspa_nosource ~kind plugin_name descr_n outputs params )
      else if mono then
        ( new ladspa_mono
            ~kind:Lang.any (Utils.get_some source) plugin_name descr_n
            inputs.(0) outputs.(0) params
          :> Source.source )
      else
        ( new ladspa
            ~kind:Lang.any (Utils.get_some source) plugin_name descr_n inputs
            outputs params
          :> Source.source ))

let register_descr plugin_name descr_n d inputs outputs =
  (* We do not register plugins without outputs for now. *)
  if outputs <> [||] then register_descr plugin_name descr_n d inputs outputs

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

let register_plugin pname =
  try
    let p = Plugin.load pname in
    let descr = Descriptor.descriptors p in
    Array.iteri
      (fun n d ->
        let i, o = get_audio_ports d in
        register_descr pname n d i o)
      descr
    (* TODO: Unloading plugins makes liq segv. Don't do it for now. *)
    (* Plugin.unload p *)
  with Plugin.Not_a_plugin -> ()

let register_plugins () =
  let add plugins_dir =
    try
      let dir = Unix.opendir plugins_dir in
      try
        while true do
          let f = Unix.readdir dir in
          if f <> "." && f <> ".." then register_plugin (plugins_dir ^ "/" ^ f)
        done
      with End_of_file -> Unix.closedir dir
    with Unix.Unix_error (e, _, _) ->
      log#info "Error while loading directory %s: %s" plugins_dir
        (Unix.error_message e)
  in
  List.iter add conf_dirs#get

let () =
  Configure.at_init (fun () -> if conf_enable#get then register_plugins ())
