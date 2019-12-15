(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type mpeg2_aac = [ `AAC_LC | `HE_AAC | `HE_AAC_v2 ]
type mpeg4_aac = [ mpeg2_aac | `AAC_LD | `AAC_ELD ]
type aot = [ `Mpeg_4 of mpeg4_aac | `Mpeg_2 of mpeg2_aac ]
type bandwidth = [ `Auto | `Fixed of int ]
type bitrate_mode = [ `Constant | `Variable of int ]
type transmux = [ `Raw | `Adif | `Adts | `Latm | `Latm_out_of_band | `Loas ]

type t = {
  afterburner : bool;
  aot : aot;
  bandwidth : bandwidth;
  bitrate_mode : bitrate_mode;
  bitrate : int;
  channels : int;
  samplerate : int Lazy.t;
  sbr_mode : bool;
  transmux : transmux;
}

let string_of_aot = function
  | `Mpeg_4 `AAC_LC -> "mpeg4_aac_lc"
  | `Mpeg_4 `HE_AAC -> "mpeg4_he_aac"
  | `Mpeg_4 `HE_AAC_v2 -> "mpeg4_he_aac_v2"
  | `Mpeg_4 `AAC_LD -> "mpeg4_aac_ld"
  | `Mpeg_4 `AAC_ELD -> "mpeg4_aac_eld"
  | `Mpeg_2 `AAC_LC -> "mpeg2_aac_lc"
  | `Mpeg_2 `HE_AAC -> "mpeg2_he_aac"
  | `Mpeg_2 `HE_AAC_v2 -> "mpeg2_he_aac_v2"

let aot_of_string = function
  | "mpeg4_aac_lc" -> `Mpeg_4 `AAC_LC
  | "mpeg4_he_aac" -> `Mpeg_4 `HE_AAC
  | "mpeg4_he_aac_v2" -> `Mpeg_4 `HE_AAC_v2
  | "mpeg4_aac_ld" -> `Mpeg_4 `AAC_LD
  | "mpeg4_aac_eld" -> `Mpeg_4 `AAC_ELD
  | "mpeg2_aac_lc" -> `Mpeg_2 `AAC_LC
  | "mpeg2_he_aac" -> `Mpeg_2 `HE_AAC
  | "mpeg2_he_aac_v2" -> `Mpeg_2 `HE_AAC_v2
  | _ -> raise Not_found

let string_of_transmux = function
  | `Raw -> "raw"
  | `Adif -> "adif"
  | `Adts -> "adts"
  | `Latm -> "latm"
  | `Latm_out_of_band -> "latm_out_of_band"
  | `Loas -> "loas"

let transmux_of_string = function
  | "raw" -> `Raw
  | "adif" -> `Adif
  | "adts" -> `Adts
  | "latm" -> `Latm
  | "latm_out_of_band" -> `Latm_out_of_band
  | "loas" -> `Loas
  | _ -> raise Not_found

let to_string m =
  let br_info =
    match m.bitrate_mode with
      | `Variable vbr -> Printf.sprintf "vbr=%d" vbr
      | `Constant -> Printf.sprintf "bitrate=%d" m.bitrate
  in
  Printf.sprintf
    "%%fdkaac(afterburner=%b,aot=%S,%s,channels=%d,samplerate=%d,sbr_mode=%b,transmux=%S)"
    m.afterburner (string_of_aot m.aot) br_info m.channels
    (Lazy.force m.samplerate) m.sbr_mode
    (string_of_transmux m.transmux)

let bitrate m = m.bitrate * 1000
