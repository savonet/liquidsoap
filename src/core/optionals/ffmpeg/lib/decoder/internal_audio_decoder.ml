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

type resampler = {
  convert : Avutil.audio Avutil.frame -> Content.data option;
  drain : unit -> Content.data option;
}

module Make
    (O : Swresample.AudioData)
    (L : sig
      val lift : O.t -> Content.data
    end) =
struct
  module R = Swresample.Make (Swresample.Frame) (O)

  let create (input : Avutil.Audio.frame_format) ~layout ~rate =
    let r =
      R.create input.channel_layout ~in_sample_format:input.sample_format
        input.sample_rate layout rate
    in
    let lift data =
      let data = L.lift data in
      if Content.is_empty data then None else Some data
    in
    {
      convert = (fun frame -> lift (R.convert r frame));
      drain = (fun () -> lift (R.flush r));
    }
end

module Float =
  Make
    (Swresample.PlanarFloatArray)
    (struct
      let lift data = Content.Audio.lift_data data
    end)

module S16 =
  Make
    (Swresample.S16PlanarBigArray)
    (struct
      let lift data = Content_pcm_s16.lift_data data
    end)

module F32 =
  Make
    (Swresample.FltPlanarBigArray)
    (struct
      let lift data = Content_pcm_f32.lift_data data
    end)

let builder pcm_kind =
  if Content.Audio.is_kind pcm_kind then Float.create
  else if Content_pcm_s16.is_kind pcm_kind then S16.create
  else if Content_pcm_f32.is_kind pcm_kind then F32.create
  else failwith "Unsupported PCM kind for the FFmpeg audio decoder"

let params_format params : Avutil.Audio.frame_format =
  let channel_layout =
    match Avcodec.Audio.get_channel_layout params with
      | l when Avutil.Channel_layout.get_nb_channels l > 0 -> l
      | _ ->
          Avutil.Channel_layout.get_default
            (Avcodec.Audio.get_nb_channels params)
  in
  {
    sample_format = Avcodec.Audio.get_sample_format params;
    sample_rate = Avcodec.Audio.get_sample_rate params;
    channel_layout;
  }

let create ~output ~faults ~(log : Log.t) ~pcm_kind ~channels params =
  let build = builder pcm_kind in
  let layout = Avutil.Channel_layout.get_default channels in
  let rate = Lazy.Mutexed.force Frame.audio_rate in
  let input = ref (params_format params) in
  let resampler = ref (build !input ~layout ~rate) in
  let put = Option.iter output.Per_stream.put in
  let flush () = put (!resampler.drain ()) in
  let push ~position:_ (item : Per_stream.item) =
    match item with
      | `Audio_frame frame -> (
          try
            let format = Avutil.Audio.frame_format frame in
            if not (Avutil.Audio.same_frame_format format !input) then (
              flush ();
              log#important "Audio format change to %s"
                (Pixel_formats.describe_audio format);
              input := format;
              resampler := build format ~layout ~rate);
            put (!resampler.convert frame);
            (match Avutil.Frame.metadata frame with
              | [] -> ()
              | m -> output.add_metadata (Frame.Metadata.from_list m));
            Faults.delivered faults;
            `Ok
          with Avutil.Error e ->
            `Item_fault
              (Printf.sprintf "Audio frame dropped: %s"
                 (Avutil.string_of_error e)))
      | _ -> `Item_fault "Unexpected item on a decoded audio stream"
  in
  {
    Per_stream.sparse = false;
    push;
    switch = (fun ~source_id:_ ~continuing:_ -> ());
    advance = (fun _ -> ());
    flush;
  }
