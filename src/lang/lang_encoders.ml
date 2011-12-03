(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

(** Parsing locations. *)
let curpos ?pos () =
  match pos with
    | None -> Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()
    | Some (i,j) -> Parsing.rhs_start_pos i, Parsing.rhs_end_pos j

(** Errors *)

exception Error of (term*string)

let invalid t =
  match t.term with
    | Int _ | Bool _ | Float _ | String _ -> false
    | _ -> true

let generic_error t =
  if invalid t then
    match t.term with
      | Var _ -> Error (t,"variables are forbidden in encoding formats")
      | _ -> Error (t,"complex expressions are forbidden in encoding formats")
  else
    Error (t,"unknown parameter name or invalid parameter value")

(** Create a new value with an unknown type. *)
let mk ?pos e =
  let kind =
    T.fresh_evar ~level:(-1) ~pos:(Some (curpos ?pos ()))
  in
    if Lang_values.debug then
      Printf.eprintf "%s (%s): assigned type var %s\n"
        (T.print_pos (Utils.get_some kind.T.pos))
        (try Lang_values.print_term {t=kind;term=e} with _ -> "<?>")
        (T.print kind) ;
    { t = kind ; term = e }

let mk_wav params =
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
          | ("stereo",{ term = Bool b }) ->
              { f with Encoder.WAV.channels = if b then 2 else 1 }
          | ("mono",{ term = Bool b }) ->
              { f with Encoder.WAV.channels = if b then 1 else 2 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.WAV.channels = 2 }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.WAV.channels = 1 }
          | ("channels",{ term = Int c }) ->
              { f with Encoder.WAV.channels = c }
          | ("duration",{ term = Float d }) ->
              { f with Encoder.WAV.duration = Some d }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.WAV.samplerate = i }
          | ("samplesize",({ term = Int i } as t)) ->
              if i <> 8 && i <> 16 then
                raise (Error (t,"invalid sample size")) ;
              { f with Encoder.WAV.samplesize = i }
          | ("header",{ term = Bool b }) ->
              { f with Encoder.WAV.header = b }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    mk (Encoder (Encoder.WAV wav))

let mp3_base_defaults = 
    { Encoder.MP3.
        stereo = true ;
        stereo_mode = Encoder.MP3.Joint_stereo ;
        samplerate = 44100 ;
        bitrate_control = Encoder.MP3.CBR 128 ;
        internal_quality = 2;
        id3v2 = None ;
        msg_interval = 0.1 ;
        msg = Configure.vendor }

let mp3_base f =
  function
    | ("stereo",{ term = Bool b }) ->
        { f with Encoder.MP3.stereo = b }
    | ("mono",{ term = Bool b }) ->
        { f with Encoder.MP3.stereo = not b }
    | ("stereo_mode",({ term = String m } as t)) ->
        let mode =
          match m with
            | "default" -> Encoder.MP3.Default
            | "joint_stereo" -> Encoder.MP3.Joint_stereo
            | "stereo" -> Encoder.MP3.Stereo
            | _ -> raise (Error(t,"Invalid stereo mode!"))
        in
        { f with Encoder.MP3.stereo_mode = mode }
    | ("internal_quality",({ term = Int q } as t)) ->
        if q > 0 || q > 9 then
          raise (Error(t,"Internal quality must be a value between 0 and 9!"));
        { f with Encoder.MP3.internal_quality = q }
    | ("msg_interval",{ term = Float i }) ->
        { f with Encoder.MP3.msg_interval = i }
    | ("msg",{ term = String m }) ->
        { f with Encoder.MP3.msg = m }
    | ("samplerate",({ term = Int i } as t)) ->
        let allowed =
          [8000;11025;12000;16000;22050;24000;32000;44100;48000]
        in
        if not (List.mem i allowed) then
          raise (Error (t,"invalid samplerate value")) ;
        { f with Encoder.MP3.samplerate = i }
    | ("id3v2",({ term = Bool true } as t)) ->
        (match !Encoder.MP3.id3v2_export with
           | None -> raise (Error(t,"No id3v2 support available for the mp3 encoder!"))
           | Some g -> { f with Encoder.MP3.id3v2 = Some g })
    | ("id3v2",{ term = Bool false }) ->
        { f with Encoder.MP3.id3v2 = None }
    | ("",{ term = Var s }) when String.lowercase s = "mono" ->
        { f with Encoder.MP3.stereo = false }
    | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
        { f with Encoder.MP3.stereo = true }
    | (_,t) -> raise (generic_error t)

let mk_mp3_cbr params =
  let defaults =
    { mp3_base_defaults with
       Encoder.MP3.
        bitrate_control = Encoder.MP3.CBR 128 }
  in
  let set_bitrate f b = 
    match f.Encoder.MP3.bitrate_control with
      | Encoder.MP3.CBR br -> 
          { f with Encoder.MP3.bitrate_control =
                Encoder.MP3.CBR b }
      | _ -> assert false
  in
  let mp3 = 
    List.fold_left
      (fun f ->
        function
          | ("bitrate",({ term = Int i } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_bitrate f i
          | x -> mp3_base f x)
      defaults params
  in
    mk (Encoder (Encoder.MP3 mp3))

let mk_mp3_abr params =
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
          | ("bitrate",({ term = Int i } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_mean_bitrate f i
          | ("min_bitrate",({ term = Int i } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_min_bitrate f i
          | ("max_bitrate",({ term = Int i } as t)) ->
              let allowed =
                [8;16;24;32;40;48;56;64;80;96;112;128;144;160;192;224;256;320]
              in
              if not (List.mem i allowed) then
                raise (Error (t,"invalid bitrate value")) ;
              set_max_bitrate f i
          | x -> mp3_base f x)
      defaults params
  in
    mk (Encoder (Encoder.MP3 mp3))

let mk_mp3_vbr params =
  let defaults =
    { mp3_base_defaults with
       Encoder.MP3.
        bitrate_control = Encoder.MP3.VBR 4 }
  in
  let mp3 =
    List.fold_left
      (fun f ->
        function
          | ("quality",({ term = Int q } as t)) ->
              if q<0 || q>9 then
                raise (Error (t,"quality should be in [0..9]")) ;
              { f with Encoder.MP3.bitrate_control = Encoder.MP3.VBR q }
          | x -> mp3_base f x)
      defaults params
  in
    mk (Encoder (Encoder.MP3 mp3))

let mk_aacplus params =
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
          | ("channels",{ term = Int i }) ->
              { f with Encoder.AACPlus.channels = i }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.AACPlus.samplerate = i }
          | ("bitrate",{ term = Int i }) ->
              { f with Encoder.AACPlus.bitrate = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.AACPlus.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.AACPlus.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    mk (Encoder (Encoder.AACPlus aacplus))

let mk_voaacenc params =
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
          | ("channels",{ term = Int i }) ->
              { f with Encoder.VoAacEnc.channels = i }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.VoAacEnc.samplerate = i }
          | ("bitrate",{ term = Int i }) ->
              { f with Encoder.VoAacEnc.bitrate = i }
          | ("adts",{ term = Bool i }) ->
              { f with Encoder.VoAacEnc.adts = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.VoAacEnc.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.VoAacEnc.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    mk (Encoder (Encoder.VoAacEnc voaacenc))

let mk_flac_gen params =
  let defaults =
    { Encoder.Flac.
        channels = 2 ;
        samplerate = 44100 ;
        bits_per_sample = 16;
        compression = 5 }
  in
  List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i }) ->
              { f with Encoder.Flac.channels = i }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.Flac.samplerate = i }
          | ("compression",({ term = Int i } as t)) ->
              if i < 0 || i >= 8 then
                raise (Error (t,"invalid compression value")) ;
              { f with Encoder.Flac.compression = i }
          | ("bits_per_sample",({ term = Int i } as t)) ->
              if i <> 8 && i <> 16 && i <> 32 then
                raise (Error (t,"invalid bits_per_sample value")) ;
              { f with Encoder.Flac.bits_per_sample = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.Flac.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.Flac.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params

let mk_ogg_flac params = 
    Encoder.Ogg.Flac (mk_flac_gen params)

let mk_flac params = 
    mk (Encoder (Encoder.Flac (mk_flac_gen params)))

let mk_external params =
  let defaults =
    { Encoder.External.
        channels = 2 ;
        samplerate = 44100 ;
        header  = true ;
        restart_on_crash = false ;
        restart = Encoder.External.No_condition ;
        process = "" }
  in
  let ext =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int c }) ->
              { f with Encoder.External.channels = c }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.External.samplerate = i }
          | ("header",{ term = Bool h }) ->
              { f with Encoder.External.header = h }
          | ("restart_on_crash",{ term = Bool h }) ->
              { f with Encoder.External.restart_on_crash = h }
          | ("",{ term = Var s })
            when String.lowercase s = "restart_on_metadata" ->
              { f with Encoder.External.restart = Encoder.External.Metadata }
          | ("restart_after_delay",{ term = Int i }) ->
              { f with Encoder.External.restart = Encoder.External.Delay i }
          | ("process",{ term = String s }) ->
              { f with Encoder.External.process = s }
          | ("",{ term = String s }) ->
              { f with Encoder.External.process = s }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    if ext.Encoder.External.process = "" then
      raise Encoder.External.No_process ;
    mk (Encoder (Encoder.External ext))

let mk_vorbis_cbr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.CBR 128 ;
        channels = 2 ;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i }) ->
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.CBR i }
          | ("channels",{ term = Int i }) ->
              { f with Encoder.Vorbis.channels = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let mk_vorbis_abr params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.ABR (None,None,None) ;
        channels = 2 ;
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
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("bitrate",{ term = Int i }) ->
              let (x,_,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,Some i,y) }
          | ("max_bitrate",{ term = Int i }) ->
              let (x,y,_) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (x,y,Some i) }
          | ("min_bitrate",{ term = Int i }) ->
              let (_,x,y) = get_rates f in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR (Some i,x,y) }
          | ("channels",{ term = Int i }) ->
              { f with Encoder.Vorbis.channels = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let mk_vorbis params =
  let defaults =
    { Encoder.Vorbis.
        mode = Encoder.Vorbis.VBR 0.3 ;
        channels = 2 ;
        samplerate = 44100 ;
    }
  in
  let vorbis =
    List.fold_left
      (fun f ->
        function
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.Vorbis.samplerate = i }
          | ("quality",({ term = Float q } as t)) ->
              if q<(-0.2) || q>1. then
                raise (Error (t,"quality should be in [(-0.2)..1]")) ;
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("quality",({ term = Int i } as t)) ->
              if i<>0 && i<>1 then
                raise (Error (t,"quality should be in [-(0.2)..1]")) ;
              let q = float i in
              { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR q }
          | ("channels",{ term = Int i }) ->
              { f with Encoder.Vorbis.channels = i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.Vorbis.channels = 1 }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.Vorbis.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Vorbis vorbis

let mk_theora params =
  let defaults =
    {
      Encoder.Theora.
       bitrate_control    = Encoder.Theora.Quality 40 ;
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
          | ("quality",({ term = Int i } as t)) ->
              (* According to the doc, this should be a value between
               * 0 and 63. *)
              if i < 0 || i > 63 then
                raise (Error (t,"Theora quality should be in 0..63")) ;
              { f with
                  Encoder.Theora.bitrate_control = Encoder.Theora.Quality i }
          | ("bitrate",{ term = Int i }) ->
              { f with
                  Encoder.Theora.bitrate_control = Encoder.Theora.Bitrate i }
          | ("width",({ term = Int i } as t)) ->
              (* According to the doc: must be a multiple of 16, and less than 1048576. *)
              if i mod 16 <> 0 || i >= 1048576 then
                raise (Error (t,"invalid frame width value")) ;
              { f with Encoder.Theora.
                    width = Lazy.lazy_from_val i;
                    picture_width = Lazy.lazy_from_val i }
          | ("height",({ term = Int i } as t)) ->
              (* According to the doc: must be a multiple of 16, and less than 1048576. *)
              if i mod 16 <> 0 || i >= 1048576 then
                raise (Error (t,"invalid frame height value")) ;
              { f with Encoder.Theora.
                    height = Lazy.lazy_from_val i;
                    picture_height = Lazy.lazy_from_val i }
          | ("picture_width",({ term = Int i } as t)) ->
              (* According to the doc: must not be larger than width. *)
              if i > Lazy.force f.Encoder.Theora.width then
                raise (Error (t,"picture width must not be larger than width.")) ;
              { f with Encoder.Theora.picture_width = Lazy.lazy_from_val i }
          | ("picture_height",({ term = Int i } as t)) ->
              (* According to the doc: must not be larger than height. *)
              if i > Lazy.force f.Encoder.Theora.height then
                raise (Error (t,"picture height must not be larger than height.")) ;
              { f with Encoder.Theora.picture_height = Lazy.lazy_from_val i }
          | ("picture_x",({ term = Int i } as t)) ->
              (* According to the doc: must be no larger than width-picture_width 
               * or 255, whichever is smaller. *)
              if 
                i > min 
                     ((Lazy.force f.Encoder.Theora.width) - 
                      (Lazy.force f.Encoder.Theora.picture_width)) 
                     255 
              then
                raise (Error (t,"picture x must not be larger than \
                                 width - picture width or 255, \
                                 whichever is smaller.")) ;
              { f with Encoder.Theora.picture_x = i }
          | ("picture_y",({ term = Int i } as t)) ->
              (* According to the doc: must be no larger than width-picture_width   
               * and frame_height-pic_height-pic_y must be no larger than 255. *)
              if 
                i > ((Lazy.force f.Encoder.Theora.height) - 
                     (Lazy.force f.Encoder.Theora.picture_height)) 
              then
                raise (Error (t,"picture y must not be larger than height - \
                                 picture height."));
              if (Lazy.force f.Encoder.Theora.picture_height) - i > 255 then
                raise (Error (t,"picture height - picture y must not be \
                                 larger than 255.")) ;
              { f with Encoder.Theora.picture_y = i }
          | ("aspect_numerator",{ term = Int i }) ->
              { f with Encoder.Theora.aspect_numerator = i }
          | ("aspect_denominator",{ term = Int i }) ->
              { f with Encoder.Theora.aspect_denominator = i }
          | ("keyframe_frequency",{ term = Int i }) ->
              { f with Encoder.Theora.keyframe_frequency = i }
          | ("vp3_compatible",{ term = Bool i }) ->
              { f with Encoder.Theora.vp3_compatible = Some i }
          | ("soft_target",{ term = Bool i }) ->
              { f with Encoder.Theora.soft_target = i }
          | ("buffer_delay",{ term = Int i }) ->
              { f with Encoder.Theora.buffer_delay = Some i }
          | ("speed",{ term = Int i }) ->
              { f with Encoder.Theora.speed = Some i }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Theora theora

let mk_dirac params =
  let defaults =
    {
      Encoder.Dirac.
       quality            = 35. ;
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
          | ("quality",{ term = Float i }) ->
              { f with Encoder.Dirac.quality = i }
          | ("width",{ term = Int i }) ->
              { f with Encoder.Dirac.
                    width = Lazy.lazy_from_val i }
          | ("height",{ term = Int i }) ->
              { f with Encoder.Dirac.
                    height = Lazy.lazy_from_val i }
          | ("aspect_numerator",{ term = Int i }) ->
              { f with Encoder.Dirac.aspect_numerator = i }
          | ("aspect_denominator",{ term = Int i }) ->
              { f with Encoder.Dirac.aspect_denominator = i }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Dirac dirac

let mk_speex params =
  let defaults =
    { Encoder.Speex.
        stereo = false ;
        samplerate = 44100 ;
        bitrate_control = Encoder.Speex.Quality 7;
        mode = Encoder.Speex.Narrowband ;
        frames_per_packet = 1 ;
        complexity = None
    }
  in
  let speex =
    List.fold_left
      (fun f ->
        function
          | ("stereo",{ term = Bool b }) ->
              { f with Encoder.Speex.stereo = b }
          | ("mono",{ term = Bool b }) ->
              { f with Encoder.Speex.stereo = not b }
          | ("samplerate",{ term = Int i }) ->
              { f with Encoder.Speex.samplerate = i }
          | ("abr",{ term = Int i }) ->
              { f with Encoder.Speex.
                        bitrate_control =
                          Encoder.Speex.Abr i }
          | ("quality",({ term = Int q } as t)) ->
            (* Doc say this should be from 0 to 10. *)
            if q < 0 || q > 10 then
              raise (Error (t,"Speex quality should be in 0..10"));
              { f with Encoder.Speex.
                        bitrate_control =
                         Encoder.Speex.Quality q }
          | ("vbr",{ term = Int q }) ->
              { f with Encoder.Speex.
                        bitrate_control =
                         Encoder.Speex.Vbr q }
          | ("mode",{ term = Var s })
            when String.lowercase s = "wideband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Wideband }
          | ("mode",{ term = Var s })
            when String.lowercase s = "narrowband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Narrowband }
          | ("mode",{ term = Var s })
            when String.lowercase s = "ultra-wideband" ->
              { f with Encoder.Speex.mode = Encoder.Speex.Ultra_wideband }
          | ("frames_per_packet",{ term = Int i }) ->
              { f with Encoder.Speex.frames_per_packet = i }
          | ("complexity",({ term = Int i } as t)) ->
              (* Doc says this should be between 1 and 10. *)
              if i < 1 || i > 10 then
                raise (Error (t,"Speex complexity should be in 1..10"));
              { f with Encoder.Speex.complexity = Some i }
          | ("",{ term = Var s }) when String.lowercase s = "mono" ->
              { f with Encoder.Speex.stereo = false }
          | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
              { f with Encoder.Speex.stereo = true }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Ogg.Speex speex
