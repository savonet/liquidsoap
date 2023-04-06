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

let load_type_of_string = function
  | "totalinit" -> `Totalinit
  | "all_settings" -> `All_settings
  | "audiofm" -> `Audiofm
  | "audio" -> `Audio
  | "processing" -> `Processing
  | "repair" -> `Repair
  | "repair_no_pnr" -> `Repair_no_pnr
  | "sublevel_pnr" -> `Sublevel_pnr
  | _ -> raise Not_found

class stereotool ~field ~preset ~load_type ~handler source =
  object (self)
    inherit Source.operator ~name:"stereotool" [source] as super
    method api_version = Stereotool.api_version handler
    method software_version = Stereotool.software_version handler

    method private load_type load_type =
      try load_type_of_string load_type
      with Not_found ->
        self#log#important "Invalid load type: %s" load_type;
        `Totalinit

    method load_preset ~load_type filename =
      Stereotool.load_preset ~load_type:(self#load_type load_type) ~filename
        handler

    method valid_license = Stereotool.valid_license handler

    val unlincensed_used_features =
      lazy (Stereotool.unlincensed_used_features handler)

    method unlincensed_used_features = Lazy.force unlincensed_used_features
    val mutable latency = None

    method latency =
      match latency with
        | Some v -> v
        | None ->
            let v =
              Frame.seconds_of_audio
                (Stereotool.latency
                   ~samplerate:(Lazy.force Frame.audio_rate)
                   ~feed_silence:true handler)
            in
            latency <- Some v;
            v

    method! wake_up l =
      super#wake_up l;
      (match preset with
        | None -> ()
        | Some filename ->
            if not (self#load_preset ~load_type filename) then
              self#log#important "Preset load failed!");
      let valid_license = self#valid_license in
      let level = if valid_license then 4 else 3 in
      self#log#f level
        "Stereotool initialized! Valid license: %b, latency: %.02fs, \
         API/software version: %d/%d"
        self#valid_license self#latency self#api_version self#software_version;
      match self#unlincensed_used_features with
        | None -> ()
        | Some s -> self#log#f level "Using unlicensed features: %s" s

    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method is_ready = source#is_ready
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
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#api_version) );
        ( "software_version",
          ([], Lang.fun_t [] Lang.int_t),
          "Software version.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#api_version) );
        ( "latency",
          ([], Lang.fun_t [] Lang.float_t),
          "Get the operator's latency.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#latency) );
        ( "valid_license",
          ([], Lang.fun_t [] Lang.bool_t),
          "Check if the license is valid for the current settings.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.bool s#valid_license) );
        ( "unlincensed_used_features",
          ([], Lang.fun_t [] (Lang.nullable_t Lang.string_t)),
          "Check if the license is valid for the current settings.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#unlincensed_used_features with
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
      let load_type = Lang.to_string (List.assoc "load_type" p) in
      let preset =
        Lang.to_valued_option Lang.to_string (List.assoc "preset" p)
      in
      let handler =
        try Stereotool.init ?license_key ~filename:library ()
        with Stereotool.Library_not_found ->
          Runtime_error.raise ~pos:(Lang.pos p)
            ~message:"Stereotool library not found or invalid!" "invalid"
      in
      let field, src = Lang.to_track (List.assoc "" p) in
      (field, new stereotool ~field ~preset ~load_type ~handler src))
