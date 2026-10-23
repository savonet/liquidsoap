(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let conf =
  Dtools.Conf.void ~p:(Decoder.conf_decoder#plug "ffmpeg") "FFmpeg decoder"

let max_interleave_duration =
  let key =
    Dtools.Conf.float
      ~p:(conf#plug "max_interleave_duration")
      "Max interleave duration" ~d:5.
      ~comments:
        [
          "Longest span, in seconds, the decoder waits for every stream of a \
           file or stream before it starts.";
        ]
  in
  key#validate (fun v -> v > 0.);
  key

let codecs =
  Dtools.Conf.void ~p:(conf#plug "codecs") "Preferred decoders"
    ~comments:["Preferred FFmpeg decoder for each codec."]

(* The decoder names of each codec, the last registered first. *)
let decoder_names (name_of_id, codec_names) =
  let table = Hashtbl.create 64 in
  List.iter
    (fun (id, name) ->
      let id = name_of_id id in
      Hashtbl.replace table id
        (name :: Option.value ~default:[] (Hashtbl.find_opt table id)))
    codec_names;
  table

let preferred = Hashtbl.create 64

let register_codec ~find (codec, names) =
  let key =
    Dtools.Conf.string ~p:(codecs#plug codec) ("Preferred decoder for " ^ codec)
  in
  key#validate (fun name ->
      match find name with _ -> true | exception _ -> false);
  ignore
    (Dtools.Conf.list ~p:(key#plug "available")
       ("Available decoders for " ^ codec)
       ~d:names);
  Hashtbl.replace preferred codec key

let () =
  let register find codec_names =
    Hashtbl.iter
      (fun codec names -> register_codec ~find (codec, names))
      (decoder_names codec_names)
  in
  register
    (fun name -> ignore (Avcodec.Audio.find_decoder_by_name name))
    ( Avcodec.Audio.string_of_id,
      List.map
        (fun c -> (Avcodec.Audio.get_id c, Avcodec.Audio.get_name c))
        Avcodec.Audio.decoders );
  register
    (fun name -> ignore (Avcodec.Video.find_decoder_by_name name))
    ( Avcodec.Video.string_of_id,
      List.map
        (fun c -> (Avcodec.Video.get_id c, Avcodec.Video.get_name c))
        Avcodec.Video.decoders );
  register
    (fun name -> ignore (Avcodec.Subtitle.find_decoder_by_name name))
    ( Avcodec.Subtitle.string_of_id,
      List.map
        (fun c -> (Avcodec.Subtitle.get_id c, Avcodec.Subtitle.get_name c))
        Avcodec.Subtitle.decoders )

let preferred_decoder codec =
  match Hashtbl.find_opt preferred codec with
    | Some key when key#is_set -> Some key#get
    | _ -> None

let default_mime_types =
  [
    "application/f4v";
    "application/ffmpeg";
    "application/mp4";
    "application/mxf";
    "application/octet-stream";
    "application/octet-stream";
    "application/ogg";
    "application/vnd.pg.format";
    "application/vnd.rn-realmedia";
    "application/vnd.smaf";
    "application/x-mpegURL";
    "application/x-ogg";
    "application/x-pgs";
    "application/x-shockwave-flash";
    "application/x-subrip";
    "application/xml";
    "audio/G722";
    "audio/MP4A-LATM";
    "audio/MPA";
    "audio/aac";
    "audio/aacp";
    "audio/aiff";
    "audio/amr";
    "audio/basic";
    "audio/bit";
    "audio/flac";
    "audio/g723";
    "audio/iLBC";
    "audio/mp4";
    "audio/mpeg";
    "audio/ogg";
    "audio/vnd.wave";
    "audio/wav";
    "audio/wave";
    "audio/webm";
    "audio/x-ac3";
    "audio/x-adpcm";
    "audio/x-caf";
    "audio/x-dca";
    "audio/x-eac3";
    "audio/x-flac";
    "audio/x-gsm";
    "audio/x-hx-aac-adts";
    "audio/x-ogg";
    "audio/x-oma";
    "audio/x-tta";
    "audio/x-voc";
    "audio/x-wav";
    "audio/x-wavpack";
    "multipart/x-mixed-replace;boundary=ffserver";
    "text/vtt";
    "text/x-ass";
    "text/x-jacosub";
    "text/x-microdvd";
    "video/3gpp";
    "video/3gpp2";
    "video/MP2T";
    "video/mp2t";
    "video/mp4";
    "video/mpeg";
    "video/ogg";
    "video/webm";
    "video/x-flv";
    "video/x-h261";
    "video/x-h263";
    "video/x-m4v";
    "video/x-matroska";
    "video/x-mjpeg";
    "video/x-ms-asf";
    "video/x-msvideo";
    "video/x-nut";
  ]

let default_image_mime_types =
  [
    "image/gif";
    "image/jpeg";
    "image/png";
    "image/vnd.microsoft.icon";
    "image/webp";
  ]

let default_file_extensions =
  [
    "264";
    "265";
    "302";
    "3g2";
    "3gp";
    "669";
    "722";
    "A64";
    "a64";
    "aa";
    "aa3";
    "aac";
    "aax";
    "ac3";
    "acm";
    "adf";
    "adp";
    "ads";
    "adts";
    "adx";
    "aea";
    "afc";
    "aif";
    "aifc";
    "aiff";
    "aix";
    "amf";
    "amr";
    "ams";
    "amv";
    "ape";
    "apl";
    "apm";
    "apng";
    "aptx";
    "aptxhd";
    "aqt";
    "asf";
    "ass";
    "ast";
    "au";
    "aud";
    "avi";
    "avr";
    "avs";
    "avs2";
    "bcstm";
    "bfstm";
    "binka";
    "bit";
    "bmv";
    "brstm";
    "c2";
    "calf";
    "cavs";
    "cdata";
    "cdg";
    "cdxl";
    "cgi";
    "chk";
    "cif";
    "cpk";
    "cvg";
    "dat";
    "daud";
    "dav";
    "dbm";
    "dif";
    "digi";
    "dmf";
    "dnxhd";
    "dnxhr";
    "drc";
    "dsm";
    "dss";
    "dtk";
    "dtm";
    "dts";
    "dtshd";
    "dv";
    "dvd";
    "eac3";
    "f4v";
    "fap";
    "far";
    "ffmeta";
    "fits";
    "flac";
    "flm";
    "flv";
    "fsb";
    "fwse";
    "g722";
    "g723_1";
    "g729";
    "gdm";
    "genh";
    "gif";
    "gsm";
    "gxf";
    "h261";
    "h263";
    "h264";
    "h265";
    "hca";
    "hevc";
    "ice";
    "ico";
    "idf";
    "idx";
    "ifv";
    "imf";
    "imx";
    "ipu";
    "ircam";
    "ism";
    "isma";
    "ismv";
    "it";
    "ivf";
    "ivr";
    "j2b";
    "jss";
    "kux";
    "latm";
    "lbc";
    "loas";
    "lrc";
    "lvf";
    "m15";
    "m1v";
    "m2a";
    "m2t";
    "m2ts";
    "m2v";
    "m3u8";
    "m4a";
    "m4b";
    "m4v";
    "mac";
    "mca";
    "mcc";
    "mdl";
    "med";
    "mj2";
    "mjpeg";
    "mjpg";
    "mk3d";
    "mka";
    "mks";
    "mkv";
    "mlp";
    "mmcmp";
    "mmf";
    "mms";
    "mo3";
    "mod";
    "mods";
    "moflex";
    "mov";
    "mp2";
    "mp3";
    "mp4";
    "mpa";
    "mpc";
    "mpd";
    "mpeg";
    "mpg";
    "mpl2";
    "mptm";
    "msbc";
    "msf";
    "mt2";
    "mtaf";
    "mtm";
    "mts";
    "musx";
    "mvi";
    "mxf";
    "mxg";
    "nist";
    "nsp";
    "nst";
    "nut";
    "obu";
    "oga";
    "ogg";
    "ogv";
    "okt";
    "oma";
    "omg";
    "opus";
    "paf";
    "pcm";
    "pjs";
    "plm";
    "psm";
    "psp";
    "pt36";
    "ptm";
    "pvf";
    "qcif";
    "ra";
    "rco";
    "rcv";
    "rgb";
    "rm";
    "roq";
    "rsd";
    "rso";
    "rt";
    "s3m";
    "sami";
    "sbc";
    "sbg";
    "scc";
    "sdr2";
    "sds";
    "sdx";
    "ser";
    "sf";
    "sfx";
    "sfx2";
    "sga";
    "shn";
    "sln";
    "smi";
    "son";
    "sox";
    "spdif";
    "sph";
    "spx";
    "srt";
    "ss2";
    "ssa";
    "st26";
    "stk";
    "stl";
    "stm";
    "stp";
    "str";
    "sub";
    "sup";
    "svag";
    "svs";
    "swf";
    "tak";
    "tco";
    "thd";
    "ts";
    "tta";
    "ttml";
    "tun";
    "txt";
    "ty";
    "ty+";
    "ult";
    "umx";
    "v";
    "v210";
    "vag";
    "vb";
    "vc1";
    "vc2";
    "viv";
    "vob";
    "voc";
    "vpk";
    "vqe";
    "vqf";
    "vql";
    "vtt";
    "w64";
    "wav";
    "webm";
    "wma";
    "wmv";
    "wow";
    "wsd";
    "wtv";
    "wv";
    "xl";
    "xm";
    "xml";
    "xmv";
    "xpk";
    "xvag";
    "y4m";
    "yop";
    "yuv";
  ]

let default_image_file_extensions =
  [
    "bmp";
    "cri";
    "dds";
    "dng";
    "dpx";
    "exr";
    "im1";
    "im24";
    "im32";
    "im8";
    "j2c";
    "j2k";
    "jls";
    "jp2";
    "jpc";
    "jpeg";
    "jpg";
    "jps";
    "ljpg";
    "mng";
    "mpg1-img";
    "mpg2-img";
    "mpg4-img";
    "mpo";
    "pam";
    "pbm";
    "pcd";
    "pct";
    "pcx";
    "pfm";
    "pgm";
    "pgmyuv";
    "pic";
    "pict";
    "pix";
    "png";
    "pnm";
    "pns";
    "ppm";
    "ptx";
    "ras";
    "raw";
    "rs";
    "sgi";
    "sun";
    "sunras";
    "svg";
    "svgz";
    "tga";
    "tif";
    "tiff";
    "webp";
    "xbm";
    "xface";
    "xpm";
    "xwd";
    "y";
    "yuv10";
  ]

let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "MIME types for media decoded by FFmpeg" ~d:default_mime_types

let image_mime_types =
  Dtools.Conf.list ~p:(mime_types#plug "images")
    "MIME types for images decoded by FFmpeg" ~d:default_image_mime_types

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions for media decoded by FFmpeg" ~d:default_file_extensions

let image_file_extensions =
  Dtools.Conf.list
    ~p:(file_extensions#plug "images")
    "File extensions for images decoded by FFmpeg"
    ~d:default_image_file_extensions

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "ffmpeg")
    "Priority of the FFmpeg decoder" ~d:10

let image_priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_image_priorities#plug "ffmpeg")
    "Priority of the FFmpeg image decoder" ~d:10

let metadata_priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "ffmpeg")
    "Priority of the FFmpeg metadata decoder" ~d:1

let is_image_extension filename =
  match String.lowercase_ascii (Utils.get_ext filename) with
    | ext -> List.mem ext image_file_extensions#get
    | exception _ -> false
