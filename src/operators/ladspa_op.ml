(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class virtual base source =
object
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track
end

class virtual base_nosource =
object
  inherit source

  method stype = Infallible

  method is_ready = true

  val mutable must_fail = false

  method abort_track =
    must_fail <- true

  method remaining = -1
end

(* A plugin is created for each channel. *)
class ladspa (source:source) plugin descr input output params =
object (self)
  inherit base source

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Array.init (Fmt.channels ())
        (fun _ ->
           Descriptor.instantiate
             d
             (Fmt.samples_per_second ())
             (Fmt.samples_per_frame ()))

  initializer
    Array.iter Descriptor.activate inst

  method get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.get_float_pcm buf in
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

class ladspa_nosource plugin descr output params =
object (self)
  inherit base_nosource

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Array.init (Fmt.channels ())
        (fun _ ->
           Descriptor.instantiate
             d
             (Fmt.samples_per_second ())
             (Fmt.samples_per_frame ()))

  initializer
    Array.iter Descriptor.activate inst

  method get_frame buf =
    if must_fail then
      (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false
      )
    else
      let offset = AFrame.position buf in
      let b = AFrame.get_float_pcm buf in
      let position = AFrame.size buf in
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
class ladspa_stereo (source:source) plugin descr inputs outputs params =
object (self)
  inherit base source

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Descriptor.instantiate
        d
        (Fmt.samples_per_second ())
        (Fmt.samples_per_frame ())

  initializer
    Descriptor.activate inst

  method get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let b = AFrame.get_float_pcm buf in
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

class ladspa_stereo_nosource plugin descr outputs params =
object (self)
  inherit base_nosource

  val inst =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      Descriptor.instantiate
        d
        (Fmt.samples_per_second ())
        (Fmt.samples_per_frame ())

  initializer
    Descriptor.activate inst

  method get_frame buf =
    if must_fail then
      (
        AFrame.add_break buf (AFrame.position buf);
        must_fail <- false
      )
    else
      let offset = AFrame.position buf in
      let b = AFrame.get_float_pcm buf in
      let position = AFrame.size buf in
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

let norm_string s =
  let s =
    Pcre.substitute
      ~pat:"( *\\([^\\)]*\\)| *\\[[^\\]]*\\])"
      ~subst:(fun _ -> "") s
  in
  let s = Pcre.substitute ~pat:"(\\.+|\\++)" ~subst:(fun _ -> "") s in
  let s = Pcre.substitute ~pat:" +$" ~subst:(fun _ -> "") s in
  let s = Pcre.substitute ~pat:"( +|/+|-+)" ~subst:(fun _ -> "_") s in
  let s = String.lowercase s in
    s

let register_descr ?(stereo=false) plugin_name descr_n d inputs outputs =
  let ports = Descriptor.port_count d in
  let control_ports =
    let ans = ref [] in
      for i = 0 to ports - 1 do
        if Descriptor.port_is_control d i && Descriptor.port_is_input d i then
          ans := i :: !ans;
      done;
      List.rev !ans
  in
  let liq_params =
    let univ = ref 0 in
      List.map
        (fun p ->
           let t = port_t d p in
             incr univ;
             norm_string (Descriptor.port_name d p),
             (match t with
                | Float -> Lang.float_getter_t !univ
                | Int -> Lang.int_t
                | Bool -> Lang.bool_t
             ),
             (match
                Descriptor.port_get_default d
                  ~samplerate:(Fmt.samples_per_second ()) p
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
                   ~samplerate:(Fmt.samples_per_second ()) p
               in
               let max =
                 Descriptor.port_get_max d
                   ~samplerate:(Fmt.samples_per_second ()) p
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
                       !bounds ^ (norm_string (Descriptor.port_name d p));
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
  let liq_params =
    liq_params@(if inputs = None then [] else ["", Lang.source_t, None, None])
  in
    Lang.add_operator ("ladspa." ^ norm_string (Descriptor.label d)) liq_params
      ~category:Lang.SoundProcessing
      ~flags:[Lang.Hidden]
      ~descr:(Descriptor.name d ^ ".")
      (fun p ->
         let f v = List.assoc v p in
         let source =
           try
             Some (Lang.to_source (f ""))
           with
             | Not_found -> None
         in
         let params =
           List.map
             (fun p ->
                p,
                let v = f (norm_string (Descriptor.port_name d p)) in
                  match port_t d p with
                    | Float -> Lang.to_float_getter v
                    | Int ->
                        let f = float_of_int (Lang.to_int v) in
                          fun () -> f
                    | Bool ->
                        let f = if Lang.to_bool v then  1. else 0. in
                          fun () -> f
             )
             control_ports
         in
           match inputs with
             | Some inputs ->
                 if stereo then
                   ((new ladspa_stereo
                       (Utils.get_some source)
                       plugin_name
                       descr_n
                       inputs
                       outputs
                       params):>source)
                 else
                   ((new ladspa
                       (Utils.get_some source)
                       plugin_name
                       descr_n
                       inputs.(0)
                       outputs.(0)
                       params):>source)
             | None ->
                 if stereo then
                   ((new ladspa_stereo_nosource
                       plugin_name
                       descr_n
                       outputs
                       params):>source)
                 else
                   ((new ladspa_nosource
                       plugin_name
                       descr_n
                       outputs.(0)
                       params):>source)
      )

(** Get the input and the output port. Raises [Not_found] if there is not
  * exactly one output and zero or one input. *)
let get_io d =
  let i, o = ref None, ref None in
  let ports = Descriptor.port_count d in
    for n = 0 to ports - 1 do
      if Descriptor.port_is_audio d n then
        if Descriptor.port_is_input d n then
          if !i <> None then
            raise Not_found
          else
            i := Some n
        else
          if !o <> None then
            raise Not_found
          else
            o := Some n
    done;
    !i,
    match !o with
      | Some o -> o
      | None -> raise Not_found

(* Same thing but for stereo I/O. *)
let get_stereo_io d =
  let i = ref None, ref None in
  let o = ref None, ref None in
  let ports = Descriptor.port_count d in
    for n = 0 to ports - 1 do
      if Descriptor.port_is_audio d n then
        if Descriptor.port_is_input d n then
          if !(fst i) = None then
            fst i := Some n
          else if !(snd i) = None then
            snd i := Some n
          else
            raise Not_found
        else
          if !(fst o) = None then
            fst o := Some n
          else if !(snd o) = None then
            snd o := Some n
          else
            raise Not_found
    done;
    match !(fst i), !(snd i), !(fst o), !(snd o) with
      | Some i_l, Some i_r, Some o_l, Some o_r ->
          (Some [|i_l; i_r|]), [|o_l; o_r|]
      | None, None, Some o_l, Some o_r -> None, [|o_l; o_r|]
      | _ -> raise Not_found

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
                     if Fmt.channels () = 2 then
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
  let plugins =
    let ans = ref [] in
    let add plugins_dir = 
      try
        let dir = Unix.opendir plugins_dir in
          try
            while true do
              let f = Unix.readdir dir in
                if f <> "." && f <> ".." then
                  ans := (plugins_dir ^ "/" ^ f) :: !ans
            done
          with
            | End_of_file -> Unix.closedir dir
      with 
        | Unix.Unix_error (e,_,_) ->
            log#f 4 "Error while loading directory %s: %s" 
              plugins_dir (Unix.error_message e)
    in
      List.iter add plugin_dirs ;
      List.rev !ans
  in
    List.iter register_plugin plugins

let () = 
  if ladspa_enable then
    register_plugins ()
