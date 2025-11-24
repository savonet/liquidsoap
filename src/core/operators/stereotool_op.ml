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

type config = {
  unlincensed_used_features : string option;
  valid_license : bool;
  latency : float;
  api_version : int;
  software_version : int;
}

class virtual base ~field ~handler (source : Source.source) =
  object (self)
    method virtual on_wake_up : (unit -> unit) -> unit
    method virtual log : Log.t

    val config =
      Lazy.from_fun (fun () ->
          (* This is computed first to inject some audio data. *)
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

    initializer
      self#on_wake_up (fun () ->
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
            | Some s -> self#log#severe "Using unlicensed features: %s" s)

    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private generate_frame =
      let b = Content.Audio.get_data (source#get_mutable_content field) in
      Stereotool.process
        ~samplerate:(Lazy.force Frame.audio_rate)
        handler b 0 source#frame_audio_position;
      source#set_frame_data field Content.Audio.lift_data b
  end

class stereotool ~field ~handler source =
  object
    inherit base ~field ~handler source
    inherit Source.operator ~name:"stereotool" [source]
  end

class active_stereotool ~field ~handler source =
  object
    inherit base ~field ~handler source
    inherit Source.operator ~name:"stereotool" [source]
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
      ( "active",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Set to `true` to keep the source streaming even when not actively \
           used by an output." );
      ("", frame_t, None, None);
    ]
    ~meth:
      [
        {
          name = "api_version";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "API version.";
          value =
            (fun s -> Lang.val_fun [] (fun _ -> Lang.int s#config.api_version));
        };
        {
          name = "software_version";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Software version.";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ -> Lang.int s#config.software_version));
        };
        {
          name = "latency";
          scheme = ([], Lang.fun_t [] Lang.float_t);
          descr = "Get the operator's latency.";
          value =
            (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#config.latency));
        };
        {
          name = "valid_license";
          scheme = ([], Lang.fun_t [] Lang.bool_t);
          descr = "Check if the license is valid for the current settings.";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ -> Lang.bool s#config.valid_license));
        };
        {
          name = "unlincensed_used_features";
          scheme = ([], Lang.fun_t [] (Lang.nullable_t Lang.string_t));
          descr = "Check if the license is valid for the current settings.";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ ->
                  match s#config.unlincensed_used_features with
                    | None -> Lang.null
                    | Some s -> Lang.string s));
        };
      ]
    ~return_t:frame_t ~category:`Audio
    ~descr:"Process the given audio track with StereoTool."
    (fun p ->
      let active = Lang.to_bool (List.assoc "active" p) in
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
                match Liquidsoap_lang.Value.pos load_type_val with
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
        try Stereotool.init ?license_key ~filename:library () with
          | Stereotool.Library_not_found ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:"Invalid stereotool library" "invalid"
          | Stereotool.Library_initialized f ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:
                  (Printf.sprintf
                     "Stereotool already initialized with a different library: \
                      %s"
                     (Lang_string.quote_string f))
                "invalid"
      in
      (match preset with
        | None -> ()
        | Some filename ->
            let pos =
              match Liquidsoap_lang.Value.pos preset_val with
                | None -> Lang.pos p
                | Some p -> [p]
            in
            if not (Stereotool.load_preset ~load_type ~filename handler) then
              Runtime_error.raise ~pos
                ~message:
                  (Printf.sprintf "Preset loading of file %S failed!" filename)
                "eval");
      let field, src = Lang.to_track (List.assoc "" p) in
      let source =
        if active then new active_stereotool ~field ~handler src
        else new stereotool ~field ~handler src
      in
      (field, source))
