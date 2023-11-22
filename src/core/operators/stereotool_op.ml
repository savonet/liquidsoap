(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type config = {
  unlincensed_used_features : string option;
  valid_license : bool;
  latency : float;
  api_version : int;
  software_version : int;
}

class stereotool ~field ~handler source =
  object (self)
    inherit Source.operator ~name:"stereotool" [source] as super

    val config =
      lazy
        ((* This is computed first to inject some audio data. *)
         let latency =
           Frame.seconds_of_audio
             (Stereotool.latency
                ~samplerate:(Lazy.force Frame.audio_rate)
                ~feed_silence:true handler)
         in
         {
           unlincensed_used_features =
             Stereotool.unlincensed_used_features handler;
           valid_license = Stereotool.valid_license handler;
           latency;
           api_version = Stereotool.api_version handler;
           software_version = Stereotool.software_version handler;
         })

    method config = Lazy.force config

    method! wake_up l =
      super#wake_up l;
      let {
        unlincensed_used_features;
        valid_license;
        latency;
        api_version;
        software_version;
      } =
        self#config
      in
      self#log#important
        "Stereotool initialized! Valid license: %b, latency: %.02fs, \
         API/software version: %d/%d"
        valid_license latency api_version software_version;
      if not valid_license then self#log#severe "Using invalid license!";
      match unlincensed_used_features with
        | None -> ()
        | Some s -> self#log#severe "Using unlicensed features: %s" s

    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method seek_source = source#seek_source
    method private _is_ready = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let position = AFrame.position buf in
      let b = Content.Audio.get_data (Frame.get buf field) in
      Stereotool.process
        ~samplerate:(Lazy.force Frame.audio_rate)
        handler b offset (position - offset)
  end

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "stereotool"
    [
      ( "library_file",
        Lang.string_t,
        None,
        Some "Path to the shared library file." );
      ("license_key", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ( "preset",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Path to a preset file to load when initializing the operator." );
      ( "load_type",
        Lang.string_t,
        Some (Lang.string "totalinit"),
        Some
          "Load type for preset. One of: \"totalinit\", \"all_settings\", \
           \"audiofm\", \"audio\", \"processing\", \"repair\", \
           \"repair_no_pnr\" or \"sublevel_pnr\"." );
      ("", frame_t, None, None);
    ]
    ~meth:
      [
        ( "api_version",
          ([], Lang.fun_t [] Lang.int_t),
          "API version.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#config.api_version) );
        ( "software_version",
          ([], Lang.fun_t [] Lang.int_t),
          "Software version.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#config.software_version)
        );
        ( "latency",
          ([], Lang.fun_t [] Lang.float_t),
          "Get the operator's latency.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#config.latency) );
        ( "valid_license",
          ([], Lang.fun_t [] Lang.bool_t),
          "Check if the license is valid for the current settings.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.bool s#config.valid_license)
        );
        ( "unlincensed_used_features",
          ([], Lang.fun_t [] (Lang.nullable_t Lang.string_t)),
          "Check if the license is valid for the current settings.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#config.unlincensed_used_features with
                  | None -> Lang.null
                  | Some s -> Lang.string s) );
      ]
    ~return_t:frame_t ~category:`Audio
    ~descr:"Process the given audio track with StereoTool."
    (fun p ->
      let library = Lang.to_string (List.assoc "library_file" p) in
      let license_key =
        Lang.to_valued_option Lang.to_string (List.assoc "license_key" p)
      in
      let license_key = if license_key = Some "" then None else license_key in
      let load_type_val = List.assoc "load_type" p in
      let load_type =
        match Lang.to_string load_type_val with
          | "totalinit" -> `Totalinit
          | "all_settings" -> `All_settings
          | "audiofm" -> `Audiofm
          | "audio" -> `Audio
          | "processing" -> `Processing
          | "repair" -> `Repair
          | "repair_no_pnr" -> `Repair_no_pnr
          | "sublevel_pnr" -> `Sublevel_pnr
          | s ->
              let pos =
                match load_type_val.Liquidsoap_lang.Value.pos with
                  | None -> Lang.pos p
                  | Some p -> [p]
              in
              Runtime_error.raise ~pos
                ~message:(Printf.sprintf "Invalid load type: %S" s)
                "invalid"
      in
      let preset_val = List.assoc "preset" p in
      let preset = Lang.to_valued_option Lang.to_string preset_val in
      let handler =
        let library = Utils.check_readable ~pos:(Lang.pos p) library in
        try Stereotool.init ?license_key ~filename:library ()
        with Stereotool.Library_not_found ->
          Runtime_error.raise ~pos:(Lang.pos p)
            ~message:"Invalid stereotool library" "invalid"
      in
      (match preset with
        | None -> ()
        | Some filename ->
            let pos =
              match preset_val.Liquidsoap_lang.Value.pos with
                | None -> Lang.pos p
                | Some p -> [p]
            in
            if not (Stereotool.load_preset ~load_type ~filename handler) then
              Runtime_error.raise ~pos
                ~message:
                  (Printf.sprintf "Preset loading of file %S failed!" filename)
                "eval");
      let field, src = Lang.to_track (List.assoc "" p) in
      (field, new stereotool ~field ~handler src))
