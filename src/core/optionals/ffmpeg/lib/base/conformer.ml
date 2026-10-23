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

module Audio = struct
  type t = {
    log : Log.t;
    format : Content.format;
    mutable resampler : (Avutil.Audio.frame_format * Helpers.Aformat.t) option;
    mutable converted : bool;
  }

  let create ~log format = { log; format; resampler = None; converted = false }

  let flush t deliver =
    match t.resampler with
      | Some (_, r) ->
          t.resampler <- None;
          Helpers.Aformat.eof r (deliver (Helpers.Aformat.time_base r))
      | None -> ()

  let resampler t ~time_base deliver (source : Avutil.Audio.frame_format) =
    match t.resampler with
      | Some (key, r) when Avutil.Audio.same_frame_format key source -> r
      | _ ->
          flush t deliver;
          let declared = Ffmpeg_content.Audio.get_params t.format in
          if not t.converted then (
            t.converted <- true;
            t.log#info "Converting raw audio from %s to %s"
              (Pixel_formats.describe_audio source)
              (Raw_params.Audio.to_string declared));
          let r =
            Helpers.Aformat.create ?sample_format:declared.sample_format
              ?channel_layout:declared.channel_layout
              ?sample_rate:declared.sample_rate ~time_base source
          in
          t.resampler <- Some (source, r);
          r

  let convert t ~time_base deliver frame =
    let params = Raw_params.Audio.of_frame frame in
    if
      Raw_params.Audio.compatible
        (Ffmpeg_content.Audio.get_params t.format)
        params
    then (
      flush t deliver;
      Content.merge t.format (Ffmpeg_content.Audio.lift_params params);
      deliver time_base frame)
    else (
      let r =
        resampler t ~time_base deliver (Avutil.Audio.frame_format frame)
      in
      Helpers.Aformat.convert r (deliver (Helpers.Aformat.time_base r)) frame)
end

module Video = struct
  type t = {
    log : Log.t;
    format : Content.format;
    mutable fit : (Helpers.Fit.target * Helpers.Fit.t) option;
  }

  let create ~log format = { log; format; fit = None }

  let target (declared : Raw_params.Video.t) (params : Raw_params.Video.t) =
    let pick a b = match a with Some _ -> a | None -> b in
    {
      Helpers.Fit.width = Option.get (pick declared.width params.width);
      height = Option.get (pick declared.height params.height);
      pixel_format = Option.get (pick declared.pixel_format params.pixel_format);
      pixel_aspect = pick declared.pixel_aspect params.pixel_aspect;
    }

  let fit t (target : Helpers.Fit.target) =
    match t.fit with
      | Some (key, f) when key = target -> f
      | previous ->
          (match previous with
            | None ->
                t.log#info "Fitting raw video to %dx%d" target.width
                  target.height
            | Some _ -> ());
          let f = Helpers.Fit.create target in
          t.fit <- Some (target, f);
          f

  let convert t deliver frame =
    let params = Raw_params.Video.of_frame frame in
    let declared = Ffmpeg_content.Video.get_params t.format in
    if Raw_params.Video.compatible declared params then (
      Content.merge t.format (Ffmpeg_content.Video.lift_params params);
      deliver frame)
    else (
      let target = target declared params in
      Content.merge t.format
        (Ffmpeg_content.Video.lift_params
           {
             Raw_params.Video.width = Some target.width;
             height = Some target.height;
             pixel_format = Some target.pixel_format;
             pixel_aspect = target.pixel_aspect;
           });
      List.iter deliver (Helpers.Fit.convert (fit t target) frame))
end
