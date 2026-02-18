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

type audio_params = {
  codec_name : string;
  codec_params : Avutil.audio Avcodec.params;
  samplerate : int;
  channels : int;
  channel_layout : string;
}

type video_params = {
  codec_name : string;
  codec_params : Avutil.video Avcodec.params;
  width : int;
  height : int;
  pixel_format : string;
  frame_rate : Avutil.rational option;
}

type subtitle_params = {
  codec_name : string;
  codec_params : Avutil.subtitle Avcodec.params;
}

type data_params = {
  codec_name : string;
  codec_params : [ `Data ] Avcodec.params;
}

type stream_params =
  [ `Audio of audio_params
  | `Video of video_params
  | `Subtitle of subtitle_params
  | `Data of data_params ]

type stream = {
  field : Frame.Fields.field;
  params : stream_params;
  copy : bool;
}

type container = { format : string option; streams : stream list }

type result = {
  content_type : Frame.content_type;
  container : container;
  description : string;
}

val audio_params_of_codec_params : Avutil.audio Avcodec.params -> audio_params

val video_params_of_codec_params :
  frame_rate:Avutil.rational option ->
  Avutil.video Avcodec.params ->
  video_params

val subtitle_params_of_codec_params :
  Avutil.subtitle Avcodec.params -> subtitle_params

val data_params_of_codec_params : [ `Data ] Avcodec.params -> data_params

val get_type :
  ?format:(Avutil.input, _) Avutil.format ->
  ctype:Frame.content_type ->
  url:string ->
  Avutil.input Avutil.container ->
  result

val json_of_container : container -> Json.t
