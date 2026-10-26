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

let pull handler deliver =
  let rec go () =
    match handler () with
      | frame ->
          deliver frame;
          go ()
      | exception Avutil.Error (`Eagain | `Eof) -> ()
  in
  go ()

let rational { Avutil.num; den } = `Rational { Avutil.num; den }

module Chain = struct
  type t = Avfilter.Utils.video_chain

  let create ~time_base format filters =
    Avfilter.Utils.video_chain ~time_base format filters

  let time_base (t : t) = Avfilter.time_base t.chain_sink.context

  let convert (t : t) deliver frame =
    t.chain_source (`Frame frame);
    pull t.chain_sink.handler deliver

  let eof (t : t) deliver =
    t.chain_source `Flush;
    pull t.chain_sink.handler deliver
end

module Fps = struct
  type start = [ `None | `Pts of Int64.t | `Time of float ]
  type t = [ `Pass of Avutil.rational | `Chain of Chain.t ]

  let filters ~target (start : start) =
    let fps rest =
      ("fps", `Pair ("fps", `String (Printf.sprintf "%d/1" target)) :: rest)
    in
    match start with
      | `None ->
          [
            ("setpts", [`Pair ("expr", `String "PTS-max(STARTPTS\\, 0)")]);
            fps [`Pair ("start_time", `Int 0)];
          ]
      | `Pts s ->
          [
            ( "setpts",
              [`Pair ("expr", `String (Printf.sprintf "%Ld+PTS-STARTPTS" s))] );
            fps [];
          ]
      | `Time t -> [fps [`Pair ("start_time", `Float t)]]

  let create ?source_rate ~target ~start ~time_base format : t =
    match source_rate with
      | Some { Avutil.num; den } when den > 0 && num = target * den ->
          `Pass time_base
      | _ -> `Chain (Chain.create ~time_base format (filters ~target start))

  let time_base = function `Pass tb -> tb | `Chain c -> Chain.time_base c

  let convert t deliver frame =
    match t with
      | `Pass _ -> deliver frame
      | `Chain c -> Chain.convert c deliver frame

  let eof t deliver =
    match t with `Pass _ -> () | `Chain c -> Chain.eof c deliver
end

module Fit = struct
  type target = {
    width : int;
    height : int;
    pixel_format : Avutil.Pixel_format.t;
    pixel_aspect : Avutil.rational option;
  }

  let square = { Avutil.num = 1; den = 1 }

  let fitted_size ?(pixel_aspect = square) ?(source_aspect = square) ~width
      ~height (source_width, source_height) =
    let num =
      source_width * source_aspect.Avutil.num * pixel_aspect.Avutil.den
    in
    let den =
      source_height * source_aspect.Avutil.den * pixel_aspect.Avutil.num
    in
    if num * height <= width * den then (max 1 (height * num / den), height)
    else (width, max 1 (width * den / num))

  type t = {
    target : target;
    mutable chain : (Avutil.Video.frame_format * Chain.t) option;
  }

  let create target = { target; chain = None }

  let filters target (format : Avutil.Video.frame_format) =
    let known_aspect =
      match target.pixel_aspect with
        | Some a -> Some a
        | None -> format.pixel_aspect
    in
    let w, h =
      fitted_size ?pixel_aspect:known_aspect ?source_aspect:format.pixel_aspect
        ~width:target.width ~height:target.height
        (format.width, format.height)
    in
    [
      ( "scale",
        [
          `Pair ("w", `Int w);
          `Pair ("h", `Int h);
          `Pair ("flags", `String Settings.scaling_algorithm#get);
        ] );
      ( "pad",
        [
          `Pair ("w", `Int target.width);
          `Pair ("h", `Int target.height);
          `Pair ("x", `String "(ow-iw)/2");
          `Pair ("y", `String "(oh-ih)/2");
        ] );
      ( "setsar",
        [
          `Pair
            ( "sar",
              match known_aspect with
                | Some a -> rational a
                | None -> `Rational { Avutil.num = 0; den = 1 } );
        ] );
      ( "format",
        [
          `Pair
            ( "pix_fmts",
              `String
                (Option.value ~default:"yuv420p"
                   (Avutil.Pixel_format.to_string target.pixel_format)) );
        ] );
    ]

  let chain t format =
    match t.chain with
      | Some (f, c) when Pixel_formats.same_video f format -> c
      | _ ->
          let c =
            Chain.create ~time_base:(Time_base.main ()) format
              (filters t.target format)
          in
          t.chain <- Some (format, c);
          c

  (* Scaling drops some colour properties: they are set back so that a
     consumer built for the fitted format sees no second change. *)
  let restore_color (source : Avutil.Video.frame_format) frame =
    Avutil.Video.frame_set_colors frame source;
    frame

  let convert t frame =
    let format = Avutil.Video.frame_format frame in
    let c = chain t format in
    let frames = ref [] in
    Chain.convert c (fun f -> frames := restore_color format f :: !frames) frame;
    List.rev !frames
end

module Aformat = struct
  type t =
    [ `Pass of Avutil.rational | `Converter of Avfilter.Utils.audio_converter ]

  let create ?sample_format ?channel_layout ?sample_rate ~time_base
      (source : Avutil.Audio.frame_format) : t =
    let out =
      {
        Avfilter.Utils.sample_rate =
          Option.value ~default:source.sample_rate sample_rate;
        channel_layout =
          Option.value ~default:source.channel_layout channel_layout;
        sample_format = Option.value ~default:source.sample_format sample_format;
      }
    in
    let same =
      out.sample_rate = source.sample_rate
      && out.sample_format = source.sample_format
      &&
        try
          Avutil.Channel_layout.compare out.channel_layout source.channel_layout
        with _ -> false
    in
    if same then `Pass time_base
    else
      `Converter
        (Avfilter.Utils.init_audio_converter ~out_params:out
           ~in_time_base:time_base
           ~in_params:
             {
               Avfilter.Utils.sample_rate = source.sample_rate;
               channel_layout = source.channel_layout;
               sample_format = source.sample_format;
             }
           ())

  let time_base = function
    | `Pass tb -> tb
    | `Converter c -> Avfilter.Utils.time_base c

  let convert t deliver frame =
    match t with
      | `Pass _ -> deliver frame
      | `Converter c -> Avfilter.Utils.convert_audio c deliver (`Frame frame)

  let eof t deliver =
    match t with
      | `Pass _ -> ()
      | `Converter c -> Avfilter.Utils.convert_audio c deliver `Flush
end
