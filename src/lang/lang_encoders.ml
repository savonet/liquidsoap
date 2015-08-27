(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

open Lang_values

(** Errors *)

exception Error of (term*string)

let invalid t =
  match t.term with
    | Int _ | Bool _ | Float _ | String _ -> false
    | _ -> true

let generic_error t : exn =
  if invalid t then
    match t.term with
      | Var _ -> Error (t,"variables are forbidden in encoding formats")
      | _ -> Error (t,"complex expressions are forbidden in encoding formats")
  else
    Error (t,"unknown parameter name or invalid parameter value")

(** Encoding format construction *)

let wav params =
  let defaults = { Encoder.WAV.
                    channels   = 2 ;
                    samplesize = 16;
                    header = true;
                    duration = None;
                    samplerate = 44100 } in
  let wav =
    List.fold_left
      (fun f ->
        function
          | ("stereo",{ term = Bool b; _ }) ->
              { f with Encoder.WAV.channels = if b then 2 else 1 }
          | ("mono",{ term = Bool b; _ }) ->
              { f with Encoder.WAV.channels = if b then 1 else 2 }
          | ("",{ term = Var s; _ }) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.WAV.channels = 2 }
          | ("",{ term = Var s; _ }) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.WAV.channels = 1 }
          | ("channels",{ term = Int c; _ }) ->
              { f with Encoder.WAV.channels = c }
          | ("duration",{ term = Float d; _ }) ->
              { f with Encoder.WAV.duration = Some d }
          | ("samplerate",{ term = Int i; _ }) ->
              { f with Encoder.WAV.samplerate = i }
          | ("samplesize",({ term = Int i; _ } as t)) ->
              if i <> 8 && i <> 16 && i <> 24 && i <> 32 then
                raise (Error (t,"invalid sample size")) ;
              { f with Encoder.WAV.samplesize = i }
          | ("header",{ term = Bool b; _ }) ->
              { f with Encoder.WAV.header = b }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.WAV wav

let avi params =
  let defaults =
    {
      Encoder.AVI.
      channels = 2;
      samplerate = 44100
    }
  in
  let avi =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int c; _ }) ->
              { f with Encoder.AVI.channels = c }
          | ("samplerate",{ term = Int i; _ }) ->
              { f with Encoder.AVI.samplerate = i }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
  Encoder.AVI avi
      
let mp3_base_defaults = 
    { Encoder.MP3.
        stereo = true ;
        stereo_mode = Encoder.MP3.Joint_stereo ;
        samplerate = 44100 ;
        bitrate_control = Encoder.MP3.CBR 128 ;
        internal_quality = 2;
        id3v2 = None ;
        msg_interval = 0.1 ;
        msg = "" }

let mp3_base f =
  function
    | ("stereo",{ term = Bool b; _ }) ->
        { f with Encoder.MP3.stereo = b }
    | ("mono",{ term = Bool b; _ }) ->
        { f with Encoder.MP3.stereo = not b }
    | ("stereo_mode",({ term = String m; _ } as t)) ->
        let mode =
          match m with
            | "default" -> Encoder.MP3.Default
            | "joint_stereo" -> Encoder.MP3.Joint_stereo
            | "stereo" -> Encoder.MP3.Stereo
            | _ -> raise (Error(t,"invalid stereo mode"))
        in
        { f with Encoder.MP3.stereo_mode = mode }
    | ("internal_quality",({ term = Int q; _ } as t)) ->
        if q < 0 || q > 9 then
          raise (Error(t,"internal quality must be a value between 0 and 9"));
        { f with Encoder.MP3.internal_quality = q }
    | ("msg_interval",{ term = Float i; _ }) ->
        { f with Encoder.MP3.msg_interval = i }
    | ("msg",{ term = String m; _ }) ->
        { f with Encoder.MP3.msg = m }
    | ("samplerate",({ term = Int i; _ } as t)) ->
        let allowed =
          [8000;11025;12000;16000;22050;24000;32000;44100;48000]
        in
        if not (List.mem i allowed) then
          raise (Error (t,"invalid samplerate value")) ;
        { f with Encoder.MP3.samplerate = i }
    | ("id3v2",({ term = Bool true; _ } as t)) ->
        (match !Encoder.MP3.id3v2_export with
           | None -> raise (Error(t,"no id3v2 support available for the mp3 encoder"))
           | Some g -> { f with Encoder.MP3.id3v2 = Some g })
    | ("id3v2",{ term = Bool false; _ }) ->
        { f with Encoder.MP3.id3v2 = None }
    | ("",{ term = Var s; _ }) when Utils.StringCompat.lowercase_ascii s = "mono" ->
        { f with Encoder.MP3.stereo = false }
    | ("",{ term = Var s; _ }) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
        { f with Encoder.MP3.stereo = true }
    | (_,t) -> raise (generic_error t)

let mp3_cbr params =
  let defaults =
    { mp3_base_defaults with
       Encoder.MP3.
        bitrate_control = Encoder.MP3.CBR 128 }
  in
  let set_bitrate f b = 
    match f.Encoder.MP3.bitrate_control with
      | Encoder.MP3.CBR _ -> 
          { f with Encoder.MP3.bitrate_control =
                Encoder.MP3.CBR b }
      | _ -> assert false
  in
  let mp3 = 
    List.fold_left
      (fun f ->
        function
          | ("bitrate",({ term = Int i; _ } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_bitrate f i
          | x -> mp3_base f x)
      defaults params
  in
    Encoder.MP3 mp3

let mp3_abr params =
  let defaults =
    { mp3_base_defaults with
       Encoder.MP3.
        bitrate_control =
         Encoder.MP3.ABR
           { Encoder.MP3.
              min_bitrate = None ;
              mean_bitrate = 128 ;
              max_bitrate = None ;
              hard_min = false } }
  in
  let set_min_bitrate f b =
    match f.Encoder.MP3.bitrate_control with
      | Encoder.MP3.ABR abr ->
          { f with Encoder.MP3.bitrate_control =
                Encoder.MP3.ABR
                  { abr with Encoder.MP3.min_bitrate = Some b }}
      | _ -> assert false
  in
  let set_max_bitrate f b =
    match f.Encoder.MP3.bitrate_control with
      | Encoder.MP3.ABR abr ->
          { f with Encoder.MP3.bitrate_control =
                Encoder.MP3.ABR
                  { abr with Encoder.MP3.max_bitrate = Some b }}
      | _ -> assert false
  in
  let set_mean_bitrate f b =
    match f.Encoder.MP3.bitrate_control with
      | Encoder.MP3.ABR abr ->
          { f with Encoder.MP3.bitrate_control =
                Encoder.MP3.ABR
                  { abr with Encoder.MP3.mean_bitrate = b }}
      | _ -> assert false
  in
  let mp3 =
    List.fold_left
      (fun f ->
        function
          | ("bitrate",({ term = Int i; _ } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_mean_bitrate f i
          | ("min_bitrate",({ term = Int i; _ } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_min_bitrate f i
          | ("max_bitrate",({ term = Int i; _ } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_max_bitrate f i
          | x -> mp3_base f x)
      defaults params
  in
    Encoder.MP3 mp3

let mp3_vbr params =
  let defaults =
    { mp3_base_defaults with
       Encoder.MP3.
        bitrate_control = Encoder.MP3.VBR 4 }
  in
  let mp3 =
    List.fold_left
      (fun f ->
        function
          | ("quality",({ term = Int q; _} as t)) ->
              if q<0 || q>9 then
                raise (Error (t,"quality should be in [0..9]")) ;
              { f with Encoder.MP3.bitrate_control = Encoder.MP3.VBR q }
          | x -> mp3_base f x)
      defaults params
  in
    Encoder.MP3 mp3

let shine params =
  let defaults =
    { Encoder.Shine.
        channels = 2 ;
        samplerate = 44100 ;
        bitrate = 128 }
  in
  let shine =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Shine.channels = i }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Shine.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.Shine.bitrate = i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Shine.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Shine.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Shine shine

let aacplus params =
  let defaults =
    { Encoder.AACPlus.
        channels = 2 ;
        samplerate = 44100 ;
        bitrate = 64 }
  in
  let aacplus =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.AACPlus.channels = i }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.AACPlus.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.AACPlus.bitrate = i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.AACPlus.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.AACPlus.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.AACPlus aacplus

let voaacenc params =
  let defaults =
    { Encoder.VoAacEnc.
        channels = 2 ;
        samplerate = 44100 ;
        bitrate = 64 ;
        adts = true }
  in
  let voaacenc =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.VoAacEnc.channels = i }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.VoAacEnc.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.VoAacEnc.bitrate = i }
          | ("adts",{ term = Bool i; _}) ->
              { f with Encoder.VoAacEnc.adts = i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.VoAacEnc.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.VoAacEnc.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.VoAacEnc voaacenc

let fdkaac params =
  let defaults =
    { Encoder.FdkAacEnc.
        afterburner    = false;
        aot            = `Mpeg_4 `HE_AAC_v2;
        bitrate        = 64;
        bitrate_mode   = `Constant;
        channels       = 2;
        samplerate     = 44100;
        sbr_mode       = false;
        transmux       = `Adts }
  in
  let valid_samplerates = [
    8000;  11025; 12000; 16000; 22050; 24000; 32000;
    44100; 48000; 64000; 88200; 96000 ]
  in
  let valid_vbr = [1;2;3;4;5] in
  let fdkaac =
    List.fold_left
      (fun f ->
        function
          | ("afterburner",{ term = Bool b; _}) ->
              { f with Encoder.FdkAacEnc.afterburner = b }
          | ("aot",({ term = String s; _} as t)) ->
              let aot = try Encoder.FdkAacEnc.aot_of_string s with
                | Not_found -> raise (Error (t,"invalid aot value"))
              in
              { f with Encoder.FdkAacEnc.aot = aot }
          | ("vbr",({ term = Int i; _} as t)) ->
              if not (List.mem i valid_vbr) then
               begin
                let err =
                  Printf.sprintf "invalid vbr mode. Possible values: %s"
                  (String.concat ", "
                    (List.map string_of_int valid_vbr))
                in
                raise (Error (t,err));
               end;
              { f with Encoder.FdkAacEnc.bitrate_mode = `Variable i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.FdkAacEnc.bitrate = i }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.FdkAacEnc.channels = i }
          | ("samplerate",({ term = Int i; _} as t)) ->
              if not (List.mem i valid_samplerates) then
               begin
                let err =
                  Printf.sprintf "invalid samplerate value. Possible values: %s"
                  (String.concat ", "
                    (List.map string_of_int valid_samplerates))
                in
                raise (Error (t,err));
               end;
              { f with Encoder.FdkAacEnc.samplerate = i }
          | ("sbr_mode",{ term = Bool b; _}) ->
              { f with Encoder.FdkAacEnc.sbr_mode = b }
          | ("transmux",({ term = String s; _} as t)) ->
              let transmux = try Encoder.FdkAacEnc.transmux_of_string s with
                | Not_found -> raise (Error (t,"invalid transmux value"))
              in
              { f with Encoder.FdkAacEnc.transmux = transmux }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.FdkAacEnc.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.FdkAacEnc.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
  let aot = fdkaac.Encoder.FdkAacEnc.aot in
    if aot = `Mpeg_4 `HE_AAC_v2 || aot = `Mpeg_2 `HE_AAC_v2 then
      if fdkaac.Encoder.FdkAacEnc.channels <> 2 then
        failwith "HE-AAC v2 is only available with 2 channels.";
    Encoder.FdkAacEnc fdkaac

let flac_gen params =
  let defaults =
    { Encoder.Flac.
        channels = 2 ;
        fill = None;
        samplerate = 44100 ;
        bits_per_sample = 16;
        compression = 5 }
  in
  List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Flac.channels = i }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Flac.samplerate = i }
          | ("compression",({ term = Int i; _} as t)) ->
              if i < 0 || i >= 8 then
                raise (Error (t,"invalid compression value")) ;
              { f with Encoder.Flac.compression = i }
          | ("bits_per_sample",({ term = Int i; _} as t)) ->
              if i <> 8 && i <> 16 && i <> 32 then
                raise (Error (t,"invalid bits_per_sample value")) ;
              { f with Encoder.Flac.bits_per_sample = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Flac.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Flac.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Flac.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params

let ogg_flac params = 
    Encoder.Ogg.Flac (flac_gen params)

let flac params = 
    Encoder.Flac (flac_gen params)

let external_encoder params =
  let defaults =
    { Encoder.External.
        channels = 2 ;
        samplerate = 44100 ;
        video = false ;
        header  = true ;
        restart_on_crash = false ;
        restart = Encoder.External.No_condition ;
        process = "" }
  in
  let ext =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int c; _}) ->
              { f with Encoder.External.channels = c }
          | ("samplerate",{ term = Int i; _}) ->
             { f with Encoder.External.samplerate = i }
          | ("video",{ term = Bool h; _}) ->
              { f with Encoder.External.video = h }
          | ("header",{ term = Bool h; _}) ->
              { f with Encoder.External.header = h }
          | ("restart_on_crash",{ term = Bool h; _}) ->
              { f with Encoder.External.restart_on_crash = h }
          | ("",{ term = Var s; _})
            when Utils.StringCompat.lowercase_ascii s = "restart_on_metadata" ->
              { f with Encoder.External.restart = Encoder.External.Metadata }
          | ("restart_after_delay",{ term = Int i; _}) ->
              { f with Encoder.External.restart = Encoder.External.Delay i }
          | ("process",{ term = String s; _}) ->
              { f with Encoder.External.process = s }
          | ("",{ term = String s; _}) ->
              { f with Encoder.External.process = s }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    if ext.Encoder.External.process = "" then
      raise Encoder.External.No_process ;
    Encoder.External ext

let opus params =
  let defaults =
    { Encoder.Opus.
        application = None ;
        complexity = None ;
        max_bandwidth = None ;
        mode = Encoder.Opus.VBR true ;
        bitrate = `Auto ;
        fill = None;
        channels = 2 ;
        samplerate = 48000 ;
        signal = None ;
        frame_size = 20.;
        dtx = false;
    }
  in
  let opus =
    List.fold_left
      (fun f ->
        function
          | ("application",{ term = String "voip"; _}) ->
              { f with Encoder.Opus.application = Some `Voip }
          | ("application",{ term = String "audio"; _}) ->
              { f with Encoder.Opus.application = Some `Audio }
          | ("application",{ term = String "restricted_lowdelay"; _}) ->
              { f with Encoder.Opus.application = Some `Restricted_lowdelay }
          | ("complexity",({ term = Int c; _} as t)) ->
              (* Doc say this should be from 0 to 10. *)
              if c < 0 || c > 10 then
                raise (Error (t,"Opus complexity should be in 0..10"));
              { f with Encoder.Opus.complexity = Some c }
          | ("max_bandwidth",{ term = String "narrow_band"; _}) ->
              { f with Encoder.Opus.max_bandwidth = Some `Narrow_band }
          | ("max_bandwidth",{ term = String "medium_band"; _}) ->
              { f with Encoder.Opus.max_bandwidth = Some `Medium_band }
          | ("max_bandwidth",{ term = String "wide_band"; _}) ->
              { f with Encoder.Opus.max_bandwidth = Some `Wide_band }
          | ("max_bandwidth",{ term = String "super_wide_band"; _}) ->
              { f with Encoder.Opus.max_bandwidth = Some `Super_wide_band }
          | ("max_bandwidth",{ term = String "full_band"; _}) ->
              { f with Encoder.Opus.max_bandwidth = Some `Full_band }
          | ( "frame_size", ({ term = Float size; _} as t)) ->
              let frame_sizes = [2.5;5.;10.;20.;40.;60.] in
              if not (List.mem size frame_sizes) then
                raise (Error (t,"Opus frame size should be one of \
                                 2.5, 5., 10., 20., 40. or 60."));
              { f with Encoder.Opus.frame_size = size }
          | ("samplerate",({ term = Int i; _} as t)) ->
              let samplerates = [8000;12000;16000;24000;48000] in
              if not (List.mem i samplerates) then
                raise (Error (t,"Opus samplerate should be one of \
                                 8000, 12000, 16000, 24000 or 48000"));
              { f with Encoder.Opus.samplerate = i }
          | ("bitrate",({ term = Int i; _} as t)) ->
              let i = i*1000 in
              (* Doc say this should be from 500 to 512000. *)
              if i < 500 || i > 512000 then
                raise (Error (t,"Opus bitrate should be in 5..512"));
              { f with Encoder.Opus.bitrate = `Bitrate i }
          | ("bitrate",{ term = String "auto"; _}) ->
              { f with Encoder.Opus.bitrate = `Auto }
          | ("bitrate",{ term = String "max"; _}) ->
              { f with Encoder.Opus.bitrate = `Bitrate_max }
          | ("channels",({ term = Int i; _} as t)) ->
              if i < 1 || i > 2 then
                raise (Error (t,"only mono and stereo streams are supported \
                                 for now"));
              { f with Encoder.Opus.channels = i }
          | ("vbr",{ term = String "none"; _}) ->
              { f with Encoder.Opus.mode = Encoder.Opus.CBR }
          | ("vbr",{ term = String "constrained"; _}) ->
              { f with Encoder.Opus.mode = Encoder.Opus.VBR true }
          | ("vbr",{ term = String "unconstrained"; _}) ->
              { f with Encoder.Opus.mode = Encoder.Opus.VBR false }
          | ("signal",{ term = String "voice"; _}) ->
              { f with Encoder.Opus.signal = Some `Voice }
          | ("signal",{ term = String "music"; _}) ->
              { f with Encoder.Opus.signal = Some `Music }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Opus.fill = Some i }
          | ("dtx",{ term = Bool b; _}) ->
              { f with Encoder.Opus.dtx = b }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Opus.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Opus.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Opus opus

let vorbis_cbr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.CBR 128 ;
        channels = 2 ;
        fill = None;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.CBR i }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let vorbis_abr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.ABR (None,None,None) ;
        channels = 2 ;
        fill = None ;
        samplerate = 44100 ;
    }
  in
  let get_rates x = 
    match x.Encoder.Vorbis.mode with
      | Encoder.Vorbis.ABR (x,y,z) -> x,y,z
      | _ -> assert false 
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              let (x,_,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,Some i,y) }
          | ("max_bitrate",{ term = Int i; _}) ->
              let (x,y,_) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,y,Some i) }
          | ("min_bitrate",{ term = Int i; _}) ->
              let (_,x,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (Some i,x,y) }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let vorbis params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.VBR 0.3 ;
        channels = 2 ;
        fill = None ;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("quality",({ term = Float q; _} as t)) ->
              if q<(-0.2) || q>1. then
                raise (Error (t,"quality should be in [(-0.2)..1]")) ;
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("quality",({ term = Int i; _} as t)) ->
              if i<>0 && i<>1 then
                raise (Error (t,"quality should be in [-(0.2)..1]")) ;
              let q = float i in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.channels = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Vorbis.fill = Some i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let theora params =
  let defaults =
    {
      Encoder.Theora.
       bitrate_control    = Encoder.Theora.Quality 40 ;
       fill               = None ;
       width              = Frame.video_width ;
       height             = Frame.video_height ;
       picture_width      = Frame.video_width ;
       picture_height     = Frame.video_height ;
       picture_x          = 0 ;
       picture_y          = 0 ;
       aspect_numerator   = 1 ;
       aspect_denominator = 1 ;
       keyframe_frequency = 64 ;
       vp3_compatible     = None ;
       soft_target        = false ;
       buffer_delay       = None ;
       speed              = None ;
    }
  in
  let theora =
    List.fold_left
      (fun f ->
        function
          | ("quality",({ term = Int i; _} as t)) ->
              (* According to the doc, this should be a value between
               * 0 and 63. *)
              if i < 0 || i > 63 then
                raise (Error (t,"Theora quality should be in 0..63")) ;
              { f with
                  Encoder.Theora.bitrate_control = Encoder.Theora.Quality i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with
                  Encoder.Theora.bitrate_control = Encoder.Theora.Bitrate i }
          | ("width",({ term = Int i; _} as t)) ->
              (* According to the doc: must be a multiple of 16, and less than 1048576. *)
              if i mod 16 <> 0 || i >= 1048576 then
                raise (Error (t,"invalid frame width value \
                                 (should be a multiple of 16)")) ;
              { f with Encoder.Theora.
                    width = Utils.LazyCompat.from_val i;
                    picture_width = Utils.LazyCompat.from_val i }
          | ("height",({ term = Int i; _} as t)) ->
              (* According to the doc: must be a multiple of 16, and less than 1048576. *)
              if i mod 16 <> 0 || i >= 1048576 then
                raise (Error (t,"invalid frame height value \
                                 (should be a multiple of 16)")) ;
              { f with Encoder.Theora.
                    height = Utils.LazyCompat.from_val i;
                    picture_height = Utils.LazyCompat.from_val i }
          | ("picture_width",({ term = Int i; _} as t)) ->
              (* According to the doc: must not be larger than width. *)
              if i > Utils.LazyCompat.force f.Encoder.Theora.width then
                raise (Error (t,"picture width must not be larger than width")) ;
              { f with Encoder.Theora.picture_width = Utils.LazyCompat.from_val i }
          | ("picture_height",({ term = Int i; _} as t)) ->
              (* According to the doc: must not be larger than height. *)
              if i > Utils.LazyCompat.force f.Encoder.Theora.height then
                raise (Error (t,"picture height must not be larger than height")) ;
              { f with Encoder.Theora.picture_height = Utils.LazyCompat.from_val i }
          | ("picture_x",({ term = Int i; _} as t)) ->
              (* According to the doc: must be no larger than width-picture_width 
               * or 255, whichever is smaller. *)
              if 
                i > min 
                     ((Utils.LazyCompat.force f.Encoder.Theora.width) - 
                      (Utils.LazyCompat.force f.Encoder.Theora.picture_width)) 
                     255 
              then
                raise (Error (t,"picture x must not be larger than \
                                 width - picture width or 255, \
                                 whichever is smaller")) ;
              { f with Encoder.Theora.picture_x = i }
          | ("picture_y",({ term = Int i; _} as t)) ->
              (* According to the doc: must be no larger than width-picture_width   
               * and frame_height-pic_height-pic_y must be no larger than 255. *)
              if 
                i > ((Utils.LazyCompat.force f.Encoder.Theora.height) - 
                     (Utils.LazyCompat.force f.Encoder.Theora.picture_height)) 
              then
                raise (Error (t,"picture y must not be larger than height - \
                                 picture height"));
              if (Utils.LazyCompat.force f.Encoder.Theora.picture_height) - i > 255 then
                raise (Error (t,"picture height - picture y must not be \
                                 larger than 255")) ;
              { f with Encoder.Theora.picture_y = i }
          | ("aspect_numerator",{ term = Int i; _}) ->
              { f with Encoder.Theora.aspect_numerator = i }
          | ("aspect_denominator",{ term = Int i; _}) ->
              { f with Encoder.Theora.aspect_denominator = i }
          | ("keyframe_frequency",{ term = Int i; _}) ->
              { f with Encoder.Theora.keyframe_frequency = i }
          | ("vp3_compatible",{ term = Bool i; _}) ->
              { f with Encoder.Theora.vp3_compatible = Some i }
          | ("soft_target",{ term = Bool i; _}) ->
              { f with Encoder.Theora.soft_target = i }
          | ("buffer_delay",{ term = Int i; _}) ->
              { f with Encoder.Theora.buffer_delay = Some i }
          | ("speed",{ term = Int i; _}) ->
              { f with Encoder.Theora.speed = Some i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Theora.fill = Some i }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Theora theora

let dirac params =
  let defaults =
    {
      Encoder.Dirac.
       quality            = 35. ;
       fill               = None ;
       width              = Frame.video_width ;
       height             = Frame.video_height ;
       aspect_numerator   = 1 ;
       aspect_denominator = 1 ;
    }
  in
  let dirac =
    List.fold_left
      (fun f ->
        function
          | ("quality",{ term = Float i; _}) ->
              { f with Encoder.Dirac.quality = i }
          | ("width",{ term = Int i; _}) ->
              { f with Encoder.Dirac.
                    width = Utils.LazyCompat.from_val i }
          | ("height",{ term = Int i; _}) ->
              { f with Encoder.Dirac.
                    height = Utils.LazyCompat.from_val i }
          | ("aspect_numerator",{ term = Int i; _}) ->
              { f with Encoder.Dirac.aspect_numerator = i }
          | ("aspect_denominator",{ term = Int i; _}) ->
              { f with Encoder.Dirac.aspect_denominator = i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Dirac.fill = Some i }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Dirac dirac

let speex params =
  let defaults =
    { Encoder.Speex.
        stereo = false ;
        fill = None ;
        samplerate = 44100 ;
        bitrate_control = Encoder.Speex.Quality 7;
        mode = Encoder.Speex.Narrowband ;
        frames_per_packet = 1 ;
        complexity = None ;
        dtx = false ;
        vad = false
    }
  in
  let speex =
    List.fold_left
      (fun f ->
        function
          | ("stereo",{ term = Bool b; _}) ->
              { f with Encoder.Speex.stereo = b }
          | ("mono",{ term = Bool b; _}) ->
              { f with Encoder.Speex.stereo = not b }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Encoder.Speex.samplerate = i }
          | ("abr",{ term = Int i; _}) ->
              { f with Encoder.Speex.
                        bitrate_control =
                          Encoder.Speex.Abr i }
          | ("quality",({ term = Int q; _} as t)) ->
            (* Doc say this should be from 0 to 10. *)
            if q < 0 || q > 10 then
              raise (Error (t,"Speex quality should be in 0..10"));
              { f with Encoder.Speex.
                        bitrate_control =
                         Encoder.Speex.Quality q }
          | ("vbr",{ term = Int q; _}) ->
              { f with Encoder.Speex.
                        bitrate_control =
                         Encoder.Speex.Vbr q }
          | ("mode",{ term = Var s; _})
            when Utils.StringCompat.lowercase_ascii s = "wideband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Wideband }
          | ("mode",{ term = Var s; _})
            when Utils.StringCompat.lowercase_ascii s = "narrowband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Narrowband }
          | ("mode",{ term = Var s; _})
            when Utils.StringCompat.lowercase_ascii s = "ultra-wideband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Ultra_wideband }
          | ("frames_per_packet",{ term = Int i; _}) ->
              { f with Encoder.Speex.frames_per_packet = i }
          | ("complexity",({ term = Int i; _} as t)) ->
              (* Doc says this should be between 1 and 10. *)
              if i < 1 || i > 10 then
                raise (Error (t,"Speex complexity should be in 1..10"));
              { f with Encoder.Speex.complexity = Some i }
          | ("bytes_per_page",{ term = Int i; _}) ->
              { f with Encoder.Speex.fill = Some i }
          | ("dtx", { term = Bool b; _}) ->
              { f with Encoder.Speex.dtx = b }
          | ("vad", { term = Bool b; _}) ->
              { f with Encoder.Speex.vad = b }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.Speex.stereo = false }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.Speex.stereo = true }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Speex speex

let gstreamer ?pos params =
  let defaults =
    { Encoder.GStreamer.
       channels  = 2;
       audio     = Some "lamemp3enc";
       has_video = true;
       video     = Some "x264enc";
       muxer     = Some "mpegtsmux";
       metadata  = "metadata";
       log        = 5;
       pipeline  = None
    }
  in
  let gstreamer =
    let perhaps = function
      | "" -> None
      | s  -> Some s
    in
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.GStreamer.channels = i }
          | ("audio",{ term = String s; _}) ->
              { f with Encoder.GStreamer.audio = perhaps s }
          | ("has_video",{ term = Bool b; _}) ->
              { f with Encoder.GStreamer.has_video = b }
          | ("video",{ term = String s; _}) ->
              { f with Encoder.GStreamer.video = perhaps s }
          | ("muxer",{ term = String s; _}) ->
              { f with Encoder.GStreamer.muxer = perhaps s }
          | ("metadata",{ term = String s; _}) ->
              { f with Encoder.GStreamer.metadata = s }
          | ("log",{ term = Int i; _}) ->
              { f with Encoder.GStreamer.log = i }
          | ("pipeline",{ term = String s; _}) ->
              { f with Encoder.GStreamer.pipeline = perhaps s }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
  let dummy =
    { Lang_values.
        t = T.fresh_evar ~level:(-1) ~pos ;
        term = Encoder (Encoder.GStreamer gstreamer) }
  in
    if gstreamer.Encoder.GStreamer.pipeline = None &&
       gstreamer.Encoder.GStreamer.audio <> None && 
       gstreamer.Encoder.GStreamer.channels = 0
    then
      raise
        (Error (dummy, "must have at least one audio channel when \
                        passing an audio pipeline"));
    if gstreamer.Encoder.GStreamer.pipeline = None &&
       gstreamer.Encoder.GStreamer.video <> None && 
       gstreamer.Encoder.GStreamer.audio <> None && 
       gstreamer.Encoder.GStreamer.muxer = None
    then
      raise
        (Error (dummy, "must have a muxer when passing an audio and \
                        a video pipeline"));
    Encoder.GStreamer gstreamer
