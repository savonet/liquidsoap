(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
open Dssi
open Dtools

let log = Log.make ["DSSI synthesizer"]

let dssi_enable =
  try
    let venv = Unix.getenv "LIQ_DSSI" in
      venv = "1" || venv = "true"
  with
    | Not_found -> true

let plugin_dirs =
  try
    let path = Unix.getenv "LIQ_DSSI_PATH" in
      Pcre.split ~pat:":" path
  with
    | Not_found -> ["/usr/lib/dssi";"/usr/local/lib/dssi"]

class dssi ~kind plugin descr outputs params source =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  val di =
    let p = Plugin.load plugin in
    let d = Descriptor.descriptor p descr in
      d,
      Ladspa.Descriptor.instantiate
        (Descriptor.ladspa d)
        (Lazy.force Frame.audio_rate)
        (AFrame.size ())

  initializer
    Ladspa.Descriptor.activate (snd di)

  method private get_frame buf =
    let descr, inst = di in
    let offset = Frame.position buf in
    let position = source#get buf ; Frame.position buf in
    let _,content = Frame.content buf offset in
    let b = content.Frame.audio in
    let evs = content.Frame.midi in
    (* Now convert everything to audio samples. *)
    let offset = Frame.audio_of_master offset in
    let position = Frame.audio_of_master position in
    let len = position - offset in
    let evs =
      let ans = ref [] in
        for c = 0 to Array.length evs - 1 do
          List.iter
            (function (t, e) ->
              let t = Frame.audio_of_master (Frame.master_of_midi t) in
              let push x = ans := (t,x) :: !ans in
                match e with
                  | Midi.Note_on (n, v) ->
                      push
                        (Dssi.Event_note_on (c, n, int_of_float (v *. 127.)))
                  | Midi.Note_off (n, v) ->
                      push
                        (Dssi.Event_note_off (c, n, int_of_float (v *. 127.)))
                  | _ -> () (* TODO *)
            ) !(evs.(c))
        done;
        Array.of_list (List.rev !ans)
    in
      List.iter
        (fun (p,v) -> Ladspa.Descriptor.connect_control_port_in inst p (v ()))
        params;
      Ladspa.Descriptor.set_samples inst len;
      assert (Array.length outputs = Array.length b) ;
      for c = 0 to Array.length outputs - 1 do
        Ladspa.Descriptor.connect_audio_port inst outputs.(c) b.(c) offset;
      done;
      Descriptor.run_synth descr inst len evs
end

let register_descr plugin_name descr_n descr outputs =
  let ladspa_descr = Descriptor.ladspa descr in
  let liq_params, params = Ladspa_op.params_of_descr ladspa_descr in
  let chans = Array.length outputs in
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained {Frame. audio = Lang.Fixed chans;
                                video = Lang.Any_fixed 0;
                                midi = Lang.Fixed 1})
  in
  let liq_params = liq_params@["", Lang.source_t k, None, None] in
    Lang.add_operator
      ("dssi." ^ Ladspa_op.norm_string (Ladspa.Descriptor.label ladspa_descr))
      liq_params
      ~kind:(Lang.Unconstrained k)
      ~category:Lang.SoundSynthesis
      ~flags:[Lang.Hidden]
      ~descr:(Ladspa.Descriptor.name ladspa_descr ^ ".")
      (fun p kind ->
         let f v = List.assoc v p in
         let source = Lang.to_source (f "") in
         let params = params p in
           new dssi ~kind plugin_name descr_n outputs params source
      )

let register_plugin pname =
  try
    let p = Plugin.load pname in
    let descr = Descriptor.descriptors p in
      Array.iteri
        (fun n d ->
           let ladspa_descr = Descriptor.ladspa d in
           let i, o = Ladspa_op.get_audio_ports ladspa_descr in
             (* TODO: we should handle inputs too someday *)
             if Array.length i = 0 then
               register_descr pname n d o
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
  if dssi_enable then
    (
      Dssi.init ();
      register_plugins ()
    )
