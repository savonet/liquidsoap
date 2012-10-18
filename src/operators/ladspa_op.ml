(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

open Source
open Ladspa
open Dtools

type t = Float | Int | Bool

let log = Log.make ["LADSPA extension"]

let ladspa_enable =
  try
    let venv = Unix.getenv "LIQ_LADSPA" in
      venv = "1" || venv = "true"
  with
    | Not_found -> true

let plugin_dirs =
  try
    let path = Unix.getenv "LIQ_LADSPA_PATH" in
      Pcre.split ~pat:":" path
  with
    | Not_found -> ["/usr/lib/ladspa";"/usr/local/lib/ladspa"]


let port_t d p =
  if Descriptor.port_is_boolean d p then Bool
  else if Descriptor.port_is_integer d p then Int
  else Float

class virtual base ~kind source =
object
  inherit operator ~name:"ladspa" kind [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track
end

class virtual base_nosource ~kind =
object
  inherit source ~name:"ladspa" kind

  method stype = Infallible

  method is_ready = true

  val mutable must_fail = false

  method abort_track =
    must_fail <- true

  method remaining = -1
end

(* A plugin is created for each channel. *)
class ladspa ~kind (source:source) plugin descr input output params =
object (self)
  inherit base ~kind source

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Array.init ((Frame.type_of_kind kind).Frame.audio)
        (fun _ ->
           Descriptor.instantiate
             d
             (Lazy.force Frame.audio_rate)
             (AFrame.size ()))

  initializer
    Array.iter Descriptor.activate inst

  method private get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.content buf offset in
    let position = AFrame.position buf in
    let len = position - offset in
      for c = 0 to Array.length b - 1 do
        Descriptor.set_samples inst.(c) len;
        Descriptor.connect_audio_port inst.(c) input b.(c) offset;
        Descriptor.connect_audio_port inst.(c) output b.(c) offset;
        List.iter
          (fun (p,v) -> Descriptor.connect_control_port_in inst.(c) p (v ()))
          params;
        Descriptor.run inst.(c)
      done
end

class ladspa_nosource ~kind plugin descr output params =
object (self)
  inherit base_nosource ~kind

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Array.init ((Frame.type_of_kind kind).Frame.audio)
        (fun _ ->
           Descriptor.instantiate
             d
             (Lazy.force Frame.audio_rate)
             (AFrame.size ()))

  initializer
    Array.iter Descriptor.activate inst

  method private get_frame buf =
    if must_fail then
      (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false
      )
    else
      let offset = AFrame.position buf in
      let b = AFrame.content buf offset in
      let position = AFrame.size () in
      let len = position - offset in
        for c = 0 to Array.length b - 1 do
          Descriptor.set_samples inst.(c) len;
          Descriptor.connect_audio_port inst.(c) output b.(c) offset;
          List.iter
            (fun (p,v) -> Descriptor.connect_control_port_in inst.(c) p (v ()))
            params;
          Descriptor.run inst.(c)
        done;
        AFrame.add_break buf position
end

(* The plugin handles stereo streams. *)
class ladspa_stereo ~kind (source:source) plugin descr inputs outputs params =
object (self)
  inherit base ~kind source

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Descriptor.instantiate
        d
        (Lazy.force Frame.audio_rate)
        (AFrame.size ())

  initializer
    Descriptor.activate inst

  method private get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.content buf offset in
    let position = AFrame.position buf in
    let len = position - offset in
      List.iter
        (fun (p,v) -> Descriptor.connect_control_port_in inst p (v ()))
        params;
      Descriptor.set_samples inst len;
      for c = 0 to 1 do
        Descriptor.connect_audio_port inst inputs.(c) b.(c) offset;
        Descriptor.connect_audio_port inst outputs.(c) b.(c) offset;
      done;
      Descriptor.run inst
end

class ladspa_stereo_nosource ~kind plugin descr outputs params =
object (self)
  inherit base_nosource ~kind

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Descriptor.instantiate
        d
        (Lazy.force Frame.audio_rate)
        (AFrame.size ())

  initializer
    Descriptor.activate inst

  method private get_frame buf =
    if must_fail then
      (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false
      )
    else
      let offset = AFrame.position buf in
      let b = AFrame.content buf offset in
      let position = AFrame.size () in
      let len = position - offset in
        List.iter
          (fun (p,v) -> Descriptor.connect_control_port_in inst p (v ()))
          params;
        Descriptor.set_samples inst len;
        for c = 0 to 1 do
          Descriptor.connect_audio_port inst outputs.(c) b.(c) offset;
        done;
        Descriptor.run inst;
        AFrame.add_break buf position
end

(* List the indexes of control ports. *)
let get_control_ports d =
  let ports = Descriptor.port_count d in
  let ans = ref [] in
    for i = 0 to ports - 1 do
      if Descriptor.port_is_control d i && Descriptor.port_is_input d i then
        ans := i :: !ans;
    done;
    List.rev !ans

(** When creating operator for LADSPA plugins, we don't know yet at
  * which samplerate liquidsoap will operate. But the default values and
  * bounds for LADSPA parameters might depend on the samplerate.
  * Lacking a better solution, we use the following default samplerate,
  * potentially creating a mismatch between the doc and the actual
  * behavior. *)
let default_samplerate = 44100

(* Make a parameter for each control port.
 * Returns the liquidsoap parameters and the parameters for the plugin. *)
let params_of_descr d =
  let control_ports = get_control_ports d in
  let liq_params =
    let univ = ref 0 in
      List.map
        (fun p ->
           let t = port_t d p in
             incr univ;
             Utils.normalize_parameter_string (Descriptor.port_name d p),
             (match t with
                | Float -> Lang.float_getter_t !univ
                | Int -> Lang.int_t
                | Bool -> Lang.bool_t
             ),
             (match
                Descriptor.port_get_default d
                  ~samplerate:default_samplerate p
              with
                | Some f ->
                    Some
                      (match t with
                         | Float -> Lang.float f
                         | Int -> Lang.int (int_of_float f)
                         | Bool -> Lang.bool (f > 0.))
                | None -> None
             ),
             let bounds =
               let min =
                 Descriptor.port_get_min d
                   ~samplerate:default_samplerate p
               in
               let max =
                 Descriptor.port_get_max d
                   ~samplerate:default_samplerate p
               in
                 if (min, max) = (None, None) then ""
                 else
                   let bounds = ref " (" in
                     begin match min with
                       | Some f ->
                           begin match t with
                             | Float ->
                                 bounds :=
                                 Printf.sprintf "%s%.6g <= " !bounds f
                             | Int ->
                                 bounds :=
                                 Printf.sprintf "%s%d <= " !bounds
                                   (int_of_float (ceil f))
                             | Bool -> ()
                           end
                       | None -> ()
                     end ;
                     bounds :=
                     !bounds ^ (Utils.normalize_parameter_string (Descriptor.port_name d p));
                     begin match max with
                       | Some f ->
                           begin match t with
                             | Float ->
                                 bounds :=
                                 Printf.sprintf "%s <= %.6g" !bounds f
                             | Int ->
                                 bounds :=
                                 Printf.sprintf "%s <= %d" !bounds
                                   (int_of_float f)
                             | Bool -> ()
                           end
                       | None -> ()
                     end ;
                     !bounds ^ ")"
             in
               Some (Descriptor.port_name d p ^ bounds ^ ".")
        )
        control_ports
  in
  let params p =
    let f v = List.assoc v p in
      List.map
        (fun p ->
           p,
           let v = f (Utils.normalize_parameter_string (Descriptor.port_name d p)) in
             match port_t d p with
               | Float -> Lang.to_float_getter v
               | Int ->
                   let f = float_of_int (Lang.to_int v) in
                     fun () -> f
                       | Bool ->
                           let f = if Lang.to_bool v then 1. else 0. in
                             fun () -> f
        )
        control_ports
  in
    liq_params, params


let register_descr ?(stereo=false) plugin_name descr_n d inputs outputs =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (if stereo then Lang.audio_stereo else Lang.any_fixed)
  in
  let liq_params, params = params_of_descr d in
  let liq_params =
    liq_params@(if inputs = None then [] else ["", Lang.source_t k, None, None])
  in
  let descr = Printf.sprintf "%s by %s." (Descriptor.name d) (Descriptor.maker d) in
    Lang.add_operator ("ladspa." ^ Utils.normalize_parameter_string (Descriptor.label d)) liq_params
      ~kind:(Lang.Unconstrained k)
      ~category:Lang.SoundProcessing
      ~flags:[Lang.Hidden]
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
           match inputs with
             | Some inputs ->
                 if stereo then
                   new ladspa_stereo ~kind
                       (Utils.get_some source)
                       plugin_name
                       descr_n
                       inputs
                       outputs
                       params
                 else
                   new ladspa ~kind
                       (Utils.get_some source)
                       plugin_name
                       descr_n
                       inputs.(0)
                       outputs.(0)
                       params
             | None ->
                 if stereo then
                   new ladspa_stereo_nosource ~kind
                       plugin_name
                       descr_n
                       outputs
                       params
                 else
                   new ladspa_nosource ~kind
                       plugin_name
                       descr_n
                       outputs.(0)
                       params
      )

(** Get input and output ports. *)
let get_audio_ports d =
  let i = ref [] in
  let o = ref [] in
  let ports = Descriptor.port_count d in
    for n = 0 to ports - 1 do
      if Descriptor.port_is_audio d n then
        if Descriptor.port_is_input d n then
          i := n :: !i
        else
          o := n :: !o
    done;
    Array.of_list (List.rev !i), Array.of_list (List.rev !o)

(** Get the input and the output port. Raises [Not_found] if there is not
  * exactly one output and zero or one input. *)
let get_io d =
  let i, o = get_audio_ports d in
    (
      if Array.length i = 0 then
        None
      else if Array.length i = 1 then
        Some i.(0)
      else
        raise Not_found
    ),
    (
      if Array.length o = 1 then
        o.(0)
      else
        raise Not_found
    )

(* Same thing but for stereo I/O. *)
let get_stereo_io d =
  let i, o = get_audio_ports d in
    (
      if Array.length i = 0 then
        None
      else if Array.length i = 2 then
        Some i
      else
        raise Not_found
    ),
    (
      if Array.length o = 2 then
        o
      else
        raise Not_found
    )

let register_plugin pname =
  try
    let p = Plugin.load pname in
    let descr = Descriptor.descriptors p in
      Array.iteri
        (fun n d ->
           try
             let i, o = get_io d in
               register_descr pname n d
                 (match i with
                    | Some i -> Some [|i|]
                    | None -> None)
                 [|o|]
           with
             | Not_found ->
                 (
                   try
                     (* TODO: handle other number of channels *)
                     let i, o = get_stereo_io d in
                       register_descr ~stereo:true pname n d i o
                   with
                     | Not_found -> ()
                 )
        ) descr
      (* TODO: Unloading plugins makes liq segv. Don't do it for now. *)
      (* Plugin.unload p *)
  with
    | Plugin.Not_a_plugin -> ()

let register_plugins () =
  let add plugins_dir =
    try
      let dir = Unix.opendir plugins_dir in
        try
          while true do
            let f = Unix.readdir dir in
              if f <> "." && f <> ".." then
                register_plugin (plugins_dir ^ "/" ^ f)
          done
        with
          | End_of_file -> Unix.closedir dir
    with
      | Unix.Unix_error (e,_,_) ->
          log#f 4 "Error while loading directory %s: %s"
            plugins_dir (Unix.error_message e)
  in
    List.iter add plugin_dirs

let () =
  if ladspa_enable then
    register_plugins ()
