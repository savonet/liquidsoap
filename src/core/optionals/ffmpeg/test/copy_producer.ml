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

(* The synthetic copy producer of spec/tests.md §3: plays the packets of a
   media file as copy content, one source id per play, laid out by the
   chosen scenario, into a [%ffmpeg] copy encoder.

   Usage: copy_producer.exe <scenario> <format> <output> <file>...

   Scenarios:
   - [exact]: positions from DTS, chunk lengths from the next packet, the last
     one the previous step, as a decoder lays them out;
   - [short]: as [exact], but each source's content ends one tick after its
     last packet, so that positions fall short of timestamps;
   - [nodur]: as [exact], with every packet duration unset;
   - [compressed]: positions at half their timestamp spacing, so that every
     stream's previous end lies beyond the next source's position;
   - [continuing]: one file split mid-GOP into two sources, the second
     marked continuing;
   - [nodts_audio], [nodts_video], [equal_dts], [backward_dts],
     [restart_dts], [close_dts]: as [exact], with the timestamps of some
     packets altered as named;
   - [tb_change]: as [exact], with the second half of each play's video in a
     run of the same source whose time base is 1/1000. *)

open Liquidsoap_ffmpeg

type scenario =
  [ `Exact
  | `Short
  | `Nodur
  | `Compressed
  | `Continuing
  | `Nodts_audio
  | `Nodts_video
  | `Equal_dts
  | `Backward_dts
  | `Restart_dts
  | `Close_dts
  | `Tb_change ]

let scenarios =
  [
    ("exact", `Exact);
    ("short", `Short);
    ("nodur", `Nodur);
    ("compressed", `Compressed);
    ("continuing", `Continuing);
    ("nodts_audio", `Nodts_audio);
    ("nodts_video", `Nodts_video);
    ("equal_dts", `Equal_dts);
    ("backward_dts", `Backward_dts);
    ("restart_dts", `Restart_dts);
    ("close_dts", `Close_dts);
    ("tb_change", `Tb_change);
  ]

let field_name = function `Audio -> "audio" | `Video -> "video"

type stream = {
  media : [ `Audio | `Video ];
  params : Copy_params.t;
  time_base : Avutil.rational;
  packets : Ffmpeg_content.copy_packet list;
}

let read_file path =
  let input = Av.open_input path in
  let audio = Av.get_audio_streams input
  and video = Av.get_video_streams input in
  let packets = Hashtbl.create 2 in
  let add index p =
    Hashtbl.replace packets index
      (p :: Option.value ~default:[] (Hashtbl.find_opt packets index))
  in
  let rec read () =
    match
      Av.read_input
        ~audio_packet:(List.map (fun (_, s, _) -> s) audio)
        ~video_packet:(List.map (fun (_, s, _) -> s) video)
        input
    with
      | `Audio_packet (i, p) ->
          add i (`Audio p);
          read ()
      | `Video_packet (i, p) ->
          add i (`Video p);
          read ()
      | _ -> read ()
      | exception Avutil.Error `Eof -> ()
  in
  read ();
  let streams =
    List.map
      (fun (i, s, p) ->
        {
          media = `Audio;
          params = Copy_params.make (`Audio p);
          time_base = Av.get_time_base s;
          packets =
            List.rev (Option.value ~default:[] (Hashtbl.find_opt packets i));
        })
      audio
    @ List.map
        (fun (i, s, p) ->
          {
            media = `Video;
            params =
              Copy_params.make ?frame_rate:(Av.get_avg_frame_rate s) (`Video p);
            time_base = Av.get_time_base s;
            packets =
              List.rev (Option.value ~default:[] (Hashtbl.find_opt packets i));
          })
        video
  in
  Av.close input;
  streams

let dts : Ffmpeg_content.copy_packet -> Int64.t option = function
  | `Audio p -> Avcodec.Packet.get_dts p
  | `Video p -> Avcodec.Packet.get_dts p
  | `Subtitle p -> Avcodec.Packet.get_dts p

let unset_duration : Ffmpeg_content.copy_packet -> Ffmpeg_content.copy_packet =
  function
  | `Audio p ->
      let p = Avcodec.Packet.dup p in
      Avcodec.Packet.set_duration p None;
      `Audio p
  | `Video p ->
      let p = Avcodec.Packet.dup p in
      Avcodec.Packet.set_duration p None;
      `Video p
  | `Subtitle p -> `Subtitle p

let with_timestamps ~dts ~pts :
    Ffmpeg_content.copy_packet -> Ffmpeg_content.copy_packet =
  let set p =
    let p = Avcodec.Packet.dup p in
    Avcodec.Packet.set_dts p dts;
    Avcodec.Packet.set_pts p pts;
    p
  in
  function
  | `Audio p -> `Audio (set p)
  | `Video p -> `Video (set p)
  | `Subtitle p -> `Subtitle (set p)

let pts : Ffmpeg_content.copy_packet -> Int64.t option = function
  | `Audio p -> Avcodec.Packet.get_pts p
  | `Video p -> Avcodec.Packet.get_pts p
  | `Subtitle p -> Avcodec.Packet.get_pts p

(* Timestamp faults of timestamps.md §6.3–§6.4, on the packets of one field;
   positions stay those of the original timestamps. *)
let retime ~(scenario : scenario) ~media packets i packet =
  let nth j = List.nth packets j in
  let shift d p = Option.map (fun t -> Int64.sub t d) p in
  match (scenario, media) with
    | `Nodts_audio, `Audio -> with_timestamps ~dts:None ~pts:(pts packet) packet
    | `Nodts_video, `Video -> with_timestamps ~dts:None ~pts:(pts packet) packet
    | `Equal_dts, `Video when i = 10 ->
        with_timestamps ~dts:(dts (nth 9)) ~pts:(pts packet) packet
    | `Backward_dts, `Video when i >= 20 && i < 23 ->
        let step =
          Int64.sub (Option.get (dts (nth 20))) (Option.get (dts (nth 17)))
        in
        with_timestamps
          ~dts:(shift step (dts packet))
          ~pts:(shift step (pts packet))
          packet
    | `Restart_dts, `Video when i >= 30 ->
        let origin = Option.get (dts (nth 30)) in
        with_timestamps
          ~dts:(shift origin (dts packet))
          ~pts:(shift origin (pts packet))
          packet
    | `Close_dts, `Video when i = 10 ->
        with_timestamps
          ~dts:(Option.map (fun t -> Int64.add t 1L) (dts (nth 9)))
          ~pts:(pts packet) packet
    | _ -> packet

let in_milliseconds ~src packet =
  let ms = { Avutil.num = 1; den = 1000 } in
  let rescale =
    Option.map (fun t ->
        Int64.of_int (Option.get (Time_base.rescale_int64 ~src ~dst:ms t)))
  in
  let set p =
    let p = Avcodec.Packet.dup p in
    Avcodec.Packet.set_dts p (rescale (Avcodec.Packet.get_dts p));
    Avcodec.Packet.set_pts p (rescale (Avcodec.Packet.get_pts p));
    Avcodec.Packet.set_duration p (rescale (Avcodec.Packet.get_duration p));
    p
  in
  match packet with
    | `Audio p -> `Audio (set p)
    | `Video p -> `Video (set p)
    | `Subtitle p -> `Subtitle (set p)

let main_of tb ts =
  Option.get (Time_base.rescale_int64 ~src:tb ~dst:(Time_base.main ()) ts)

(* One play of the file: the content of each field, one run per field after
   an empty leading run where the stream starts after the earliest one. *)
let source ?(continues = false) ~(scenario : scenario) streams =
  let source_id = Source_id.make () in
  let origin =
    List.fold_left min max_int
      (List.map
         (fun s -> main_of s.time_base (Option.get (dts (List.hd s.packets))))
         streams)
  in
  List.map
    (fun s ->
      let scale = if scenario = `Compressed then 2 else 1 in
      let positions =
        List.map
          (fun p -> (main_of s.time_base (Option.get (dts p)) - origin) / scale)
          s.packets
      in
      let first = List.hd positions in
      let last = List.nth positions (List.length positions - 1) in
      let previous_step =
        match List.rev positions with a :: b :: _ -> a - b | _ -> 1
      in
      let length =
        match scenario with
          | `Short -> last - first + 1
          | _ -> last - first + previous_step
      in
      let packets =
        if scenario = `Nodur then List.map unset_duration s.packets
        else List.mapi (retime ~scenario ~media:s.media s.packets) s.packets
      in
      let items = List.map2 (fun pos p -> (pos - first, p)) positions packets in
      let data =
        Ffmpeg_content.Copy.Data.make ~length:(first + length) (Some s.params)
      in
      let lead =
        Ffmpeg_content.Copy.Data.run ~continues ~source_id
          ~time_base:s.time_base ~source_params:s.params ~length:first []
      in
      let run ~time_base ~length items =
        Ffmpeg_content.Copy.Data.run ~continues ~source_id ~time_base
          ~source_params:s.params ~length items
      in
      let body =
        if scenario = `Tb_change && s.media = `Video then (
          let half = List.length items / 2 in
          let before = List.filteri (fun i _ -> i < half) items
          and after = List.filteri (fun i _ -> i >= half) items in
          let split = fst (List.hd after) in
          [
            (first, run ~time_base:s.time_base ~length:split before);
            ( first + split,
              run
                ~time_base:{ Avutil.num = 1; den = 1000 }
                ~length:(length - split)
                (List.map
                   (fun (pos, p) ->
                     (pos - split, in_milliseconds ~src:s.time_base p))
                   after) );
          ])
        else [(first, run ~time_base:s.time_base ~length items)]
      in
      data.runs <- (if first > 0 then [(0, lead)] else []) @ body;
      (field_name s.media, s.params, data))
    streams

(* One file split mid-GOP at [at] seconds into two sources, the second's
   timestamps restarting [at] seconds lower and its id marked continuing, as
   a decoder does at a declared discontinuity whose bitstream goes on. *)
let continuing ~at streams =
  let split s =
    let seconds p =
      Int64.to_float (Option.get (dts p))
      *. float s.time_base.Avutil.num
      /. float s.time_base.Avutil.den
    in
    let before, after = List.partition (fun p -> seconds p < at) s.packets in
    let shift =
      Int64.of_float
        (at *. float s.time_base.Avutil.den /. float s.time_base.Avutil.num)
    in
    let restart p =
      with_timestamps
        ~dts:(Option.map (fun t -> Int64.sub t shift) (dts p))
        ~pts:(Option.map (fun t -> Int64.sub t shift) (pts p))
        p
    in
    ({ s with packets = before }, { s with packets = List.map restart after })
  in
  let halves = List.map split streams in
  [
    source ~scenario:`Exact (List.map fst halves);
    source ~continues:true ~scenario:`Exact (List.map snd halves);
  ]

let () =
  Frame_settings.lazy_config_eval := true;
  if Sys.getenv_opt "COPY_PRODUCER_LOG" <> None then (
    Dtools.Log.conf_stdout#set true;
    Dtools.Log.conf_level#set 4;
    Dtools.Log.conf_file#set false;
    Dtools.Init.exec Dtools.Log.start);
  match Array.to_list Sys.argv with
    | _ :: scenario :: format :: output :: files ->
        let scenario =
          match List.assoc_opt scenario scenarios with
            | Some scenario -> scenario
            | None ->
                prerr_endline ("Unknown scenario: " ^ scenario);
                exit 2
        in
        let plays =
          match (scenario, files) with
            | `Continuing, [file] -> continuing ~at:3.4 (read_file file)
            | `Continuing, _ ->
                prerr_endline "The continuing scenario takes one file";
                exit 2
            | _ -> List.map (fun f -> source ~scenario (read_file f)) files
        in
        let fields = List.map (fun (f, p, _) -> (f, p)) (List.hd plays) in
        let ctype =
          List.fold_left
            (fun ctype (field, params) ->
              Frame.Fields.add
                (Frame.Fields.register field)
                (Ffmpeg_content.Copy.lift_params (Some params))
                ctype)
            Frame.Fields.empty fields
        in
        let generator = Generator.create ctype in
        List.iter
          (fun play ->
            let longest =
              List.fold_left
                (fun l (_, _, (d : Ffmpeg_content.Copy.Data.data)) ->
                  max l d.length)
                0 play
            in
            List.iter
              (fun (field, _, (d : Ffmpeg_content.Copy.Data.data)) ->
                d.length <- longest;
                Generator.put generator
                  (Frame.Fields.register field)
                  (Ffmpeg_content.Copy.lift_data d))
              play)
          plays;
        let format =
          {
            Liquidsoap_ffmpeg_formats.Encoder_format.format = Some format;
            mime_type = None;
            output = `Url ("file:" ^ output);
            streams =
              List.map (fun (f, _) -> (f, `Copy `Wait_for_keyframe)) fields;
            interleaved = `Default;
            metadata = [];
            opts = [];
          }
        in
        let encoder =
          Instance.create ~pos:None ~name:"copy_producer"
            Frame.Metadata.Export.empty format
        in
        let frame_size = Lazy.Mutexed.force Frame.size in
        while Generator.length generator > 0 do
          ignore (encoder.Encoder.encode (Generator.slice generator frame_size))
        done;
        ignore (encoder.Encoder.stop ())
    | _ ->
        prerr_endline
          "Usage: copy_producer.exe <scenario> <format> <output> <file>...";
        exit 2
