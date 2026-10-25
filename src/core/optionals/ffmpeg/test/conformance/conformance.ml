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

exception Check_failed of string

let assertions = ref 0

let check name cond =
  incr assertions;
  if not cond then raise (Check_failed name)

let fail fmt = Printf.ksprintf (fun s -> raise (Check_failed s)) fmt

(* Times are in seconds, [tb] included. *)
type packet = {
  dts : float option;
  pts : float option;
  duration : float option;
  key : bool;
  size : int;
  tb : float;
}

let dts p = Option.get p.dts
let pts p = Option.get p.pts
let duration p = Option.value ~default:0. p.duration
let seconds tb t = Int64.to_float t *. tb
let tb_of { Avutil.num; den } = float num /. float den

let packets path =
  let input = Av.open_input path in
  let time_bases = Hashtbl.create 4 in
  let register kind =
    List.iter (fun (i, s, _) ->
        Hashtbl.replace time_bases i (kind, tb_of (Av.get_time_base s)))
  in
  let audio = Av.get_audio_streams input
  and video = Av.get_video_streams input
  and subtitle = Av.get_subtitle_streams input in
  register `Audio audio;
  register `Video video;
  register `Subtitle subtitle;
  let streams = Hashtbl.create 4 in
  let add i p =
    let kind, tb = Hashtbl.find time_bases i in
    let time get = Option.map (seconds tb) (get p) in
    let packet =
      {
        dts = time Avcodec.Packet.get_dts;
        pts = time Avcodec.Packet.get_pts;
        duration = time Avcodec.Packet.get_duration;
        key = List.mem `Keyframe (Avcodec.Packet.get_flags p);
        size = Avcodec.Packet.get_size p;
        tb;
      }
    in
    Hashtbl.replace streams kind
      (packet :: Option.value ~default:[] (Hashtbl.find_opt streams kind))
  in
  let rec read () =
    match
      Av.read_input
        ~audio_packet:(List.map (fun (_, s, _) -> s) audio)
        ~video_packet:(List.map (fun (_, s, _) -> s) video)
        ~subtitle_packet:(List.map (fun (_, s, _) -> s) subtitle)
        input
    with
      | `Audio_packet (i, p) ->
          add i p;
          read ()
      | `Video_packet (i, p) ->
          add i p;
          read ()
      | `Subtitle_packet (i, p) ->
          add i p;
          read ()
      | _ -> read ()
      | exception Avutil.Error `Eof -> ()
  in
  read ();
  Av.close input;
  fun kind ->
    List.rev (Option.value ~default:[] (Hashtbl.find_opt streams kind))

module Resampler = Swresample.Make (Swresample.Frame) (Swresample.FloatArray)

(* Onsets of the beeps and of the white frames of a file: a sample above 0.3
   or a frame of mean luma above 128, after half a second without one. *)
let markers path =
  let input = Av.open_input path in
  let _, audio, audio_params = Av.find_best_audio_stream input in
  let _, video, _ = Av.find_best_video_stream input in
  let rate = Avcodec.Audio.get_sample_rate audio_params in
  let resampler =
    Resampler.create
      (Avcodec.Audio.get_channel_layout audio_params)
      ~in_sample_format:(Avcodec.Audio.get_sample_format audio_params)
      rate Avutil.Channel_layout.mono rate
  in
  let audio_tb = tb_of (Av.get_time_base audio)
  and video_tb = tb_of (Av.get_time_base video) in
  let onsets () = (ref [], ref neg_infinity) in
  let beeps = onsets () and flashes = onsets () in
  let mark (found, last) t =
    if t -. !last > 0.5 then found := t :: !found;
    last := t
  in
  let frame_time tb frame =
    seconds tb (Option.value ~default:0L (Avutil.Frame.pts frame))
  in
  let on_audio frame =
    let start = frame_time audio_tb frame in
    Array.iteri
      (fun i sample ->
        if Float.abs sample > 0.3 then
          mark beeps (start +. (float i /. float rate)))
      (Resampler.convert resampler frame)
  in
  let on_video frame =
    let width = Avutil.Video.frame_get_width frame
    and height = Avutil.Video.frame_get_height frame in
    let total = ref 0 in
    ignore
      (Avutil.Video.frame_visit ~make_writable:false
         (fun planes ->
           let luma, linesize = planes.(0) in
           for y = 0 to height - 1 do
             for x = 0 to width - 1 do
               total := !total + Bigarray.Array1.get luma ((y * linesize) + x)
             done
           done)
         frame);
    if !total / (width * height) > 128 then
      mark flashes (frame_time video_tb frame)
  in
  let rec read () =
    match Av.read_input ~audio_frame:[audio] ~video_frame:[video] input with
      | `Audio_frame (_, f) ->
          on_audio f;
          read ()
      | `Video_frame (_, f) ->
          on_video f;
          read ()
      | _ -> read ()
      | exception Avutil.Error `Eof -> ()
  in
  read ();
  Av.close input;
  (List.rev !(fst beeps), List.rev !(fst flashes))

(* Decoded video frames that carry a PTS or a best-effort timestamp. *)
let timed_frames path =
  let input = Av.open_input path in
  let _, video, _ = Av.find_best_video_stream input in
  let rec count n =
    match Av.read_input ~video_frame:[video] input with
      | `Video_frame (_, f) ->
          let timed =
            Avutil.Frame.pts f <> None
            || Avutil.Frame.best_effort_timestamp f <> None
          in
          count (if timed then n + 1 else n)
      | _ -> count n
      | exception Avutil.Error `Eof -> n
  in
  let n = count 0 in
  Av.close input;
  n

let wait name pid =
  match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED 0 -> `Ok
    | Unix.WEXITED n -> `Exited n
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        fail "%s: killed by signal %d" name n

let run_script name script =
  let path = name ^ ".liq" and log = name ^ ".log" in
  Out_channel.with_open_bin path (fun oc -> output_string oc script);
  let fd =
    Unix.openfile log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644
  in
  let pid =
    Unix.create_process "liquidsoap" [| "liquidsoap"; path |] Unix.stdin fd fd
  in
  Unix.close fd;
  let status = wait name pid in
  (status, In_channel.with_open_bin log In_channel.input_all)

let run_ok name script =
  match run_script name script with
    | `Ok, log -> log
    | `Exited n, log ->
        let length = min 2000 (String.length log) in
        fail "%s: liquidsoap exited with %d: %s" name n
          (String.sub log (String.length log - length) length)

(* The [<prefix...>] marks a script printed, in order. A mark is found
   wherever it lands: the log shares the script's output and can cut a line. *)
let marks ~prefix log =
  let opening = "<" ^ prefix in
  let length = String.length opening in
  let rec find from =
    if from + length > String.length log then None
    else if String.sub log from length = opening then Some from
    else find (from + 1)
  in
  let rec scan from acc =
    match find from with
      | Some start -> (
          match String.index_from_opt log start '>' with
            | Some stop ->
                scan (stop + 1)
                  (String.sub log (start + 1) (stop - start - 1) :: acc)
            | None -> List.rev acc)
      | None -> List.rev acc
  in
  scan 0 []

let producer = ref ""

let produce scenario format out sources =
  let pid =
    Unix.create_process !producer
      (Array.of_list ([!producer; scenario; format; out] @ sources))
      Unix.stdin Unix.stdout Unix.stderr
  in
  match wait "copy producer" pid with
    | `Ok -> ()
    | `Exited n -> fail "copy producer %s exited with %d" scenario n

let occurrences sub s =
  let n = String.length sub in
  let rec count i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then count (i + n) (acc + 1)
    else count (i + 1) acc
  in
  count 0 0

let contains sub s = occurrences sub s > 0

(* Every request resolves at startup, so a CPU-led clock never finds the
   next file unprepared. *)
let copy_script sources tracks out format =
  Printf.sprintf
    {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([%s])
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="%s", %s), "%s", s)
o.on_stop(synchronous=true, shutdown)
|}
    (String.concat ", "
       (List.map (Printf.sprintf {|once(single("%s"))|}) sources))
    format tracks out

(* A file rejected at open leaves the playlist to move on, which takes a
   real-time clock: a CPU-led one stops the fallible output at the gap. *)
let playlist_script name files tracks out format =
  let m3u = name ^ ".m3u" in
  Out_channel.with_open_bin m3u (fun oc ->
      List.iter (fun f -> output_string oc (f ^ "\n")) files);
  Printf.sprintf
    {|settings.decoder.decoders.set(["ffmpeg"])
s = playlist(mode="normal", loop=false, reload_mode="never", "%s")
o = output.file(fallible=true, %%ffmpeg(format="%s", %s), "%s", s)
o.on_stop(synchronous=true, shutdown)
|}
    m3u format tracks out

let live_script source tracks out =
  Printf.sprintf
    {|s = input.ffmpeg("%s")
s.on_disconnect(synchronous=true, shutdown)
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="nut", %s), "%s", s)
|}
    source tracks out

let rec pairs = function a :: (b :: _ as l) -> (a, b) :: pairs l | _ -> []
let steps l = List.map (fun (a, b) -> dts b -. dts a) (pairs l)
let rec drop n l = match l with _ :: l when n > 0 -> drop (n - 1) l | l -> l

let rec take n l =
  match l with x :: l when n > 0 -> x :: take (n - 1) l | _ -> []

let slice i n l = take n (drop i l)
let last l = List.nth l (List.length l - 1)

let first_index f l =
  fst (List.find (fun (_, x) -> f x) (List.mapi (fun i x -> (i, x)) l))

let rec zip_short a b =
  match (a, b) with x :: a, y :: b -> (x, y) :: zip_short a b | _ -> []

let latest l = List.fold_left max neg_infinity l
let timestamps l = List.map (fun p -> (p.dts, p.pts)) l
let end_of p = dts p +. duration p

(* MPEG-TS stores no packet duration: the end of [src] copied into [out] takes
   the duration of its last source packet. *)
let copied_end ~src out =
  dts (List.nth out (List.length src - 1)) +. duration (last src)

let both_orders = ["%audio.copy, %video.copy"; "%video.copy, %audio.copy"]
let repeat n file = List.init n (fun _ -> file)

let assert_monotonic name l =
  ignore
    (List.fold_left
       (fun previous p ->
         match p.dts with
           | None -> previous
           | Some d ->
               check
                 (name ^ ": DTS strictly increasing")
                 (Option.fold ~none:true ~some:(fun l -> d > l) previous);
               check (name ^ ": PTS >= DTS")
                 (Option.fold ~none:true ~some:(fun p -> p >= d) p.pts);
               Some d)
       None l)

let near ~within expected v = Float.abs (v -. expected) <= within
let first_key l = List.find_opt (fun p -> p.key) l

let fixture_properties name =
  let pkts = packets name in
  let v = pkts `Video and a = pkts `Audio in
  let start l = pts (List.hd l) in
  let stop l = latest (List.map (fun p -> pts p +. duration p) l) in
  let reorders l = List.exists (fun p -> p.pts <> p.dts) l in
  match name with
    | "bf1.mp4" | "bf2.mp4" | "bf3.mp4" ->
        let first =
          List.assoc name
            [("bf1.mp4", 41.17); ("bf2.mp4", 25.2); ("bf3.mp4", 8.17)]
        in
        [
          ("video only", a = [] && v <> []);
          ("B-frames", reorders v);
          ("first DTS", near ~within:0.1 first (dts (List.hd v)));
        ]
    | "gop.mp4" ->
        let keys = List.filter (fun p -> p.key) v in
        [
          ("audio and video", a <> [] && v <> []);
          ("a keyframe every second", List.length keys = 6);
        ]
    | "marked_midgop.mp4" ->
        let presented = List.filter (fun p -> pts p >= 0.) v in
        [
          ( "presentation starts mid-GOP",
            presented <> [] && not (List.hd presented).key );
          ( "a keyframe before the presentation start",
            (List.hd v).key && start v < 0. );
        ]
    | "midgop.mp4" -> (
        match first_key v with
          | Some k ->
              [
                ("starts mid-GOP", not (List.hd v).key);
                ("audio before the first keyframe", start a < pts k -. 0.1);
              ]
          | None -> [("has a keyframe", false)])
    | "avoff.mp4" ->
        [
          ( "audio ends 200 ms before video",
            near ~within:0.05 0.2 (stop v -. stop a) );
        ]
    | "nokey.mp4" -> [("video without keyframe", v <> [] && first_key v = None)]
    | "latekey.mp4" -> (
        match first_key v with
          | Some k ->
              [
                ( "first keyframe 25 s in",
                  near ~within:0.5 25. (pts k -. start v) );
              ]
          | None -> [("has a keyframe", false)])
    | "fmtchange.ts" -> [("180 frames", List.length v = 180)]
    | "count.mp4" -> [("75 frames", List.length v = 75)]
    | "count.wav" ->
        [
          ( "one second",
            near ~within:0.001 1.
              (List.fold_left (fun t p -> t +. duration p) 0. a) );
        ]
    | "besteffort.avi" ->
        [
          ( "video packets without PTS",
            v <> [] && List.for_all (fun p -> p.pts = None) v );
          ("B-frames", timed_frames name >= 90);
        ]
    | "latevideo.ts" ->
        [("video 8 s after audio", near ~within:0.1 8. (start v -. start a))]
    | "earlyaudio.ts" ->
        [("video 2 s after audio", near ~within:0.1 2. (start v -. start a))]
    | "sub_short.mkv" | "sub_late.mkv" | "sub_early.mkv" ->
        let cue =
          List.assoc name
            [
              ("sub_short.mkv", 1.);
              ("sub_late.mkv", 1200.);
              ("sub_early.mkv", 30.);
            ]
        in
        [
          ( "one cue",
            match pkts `Subtitle with
              | [c] -> near ~within:0.01 cue (pts c)
              | _ -> false );
        ]
    | _ -> []

(* Fixture properties of spec/tests.md §2, verified before a check uses the
   fixture. *)
let require names =
  List.iter
    (fun name ->
      List.iter
        (fun (property, holds) ->
          check (Printf.sprintf "fixture %s: %s" name property) holds)
        (fixture_properties name))
    names

(* known-complexity.md §1: rebasing a sequence of copied sources. *)
let kc1_assertions name out sources =
  check (name ^ ": packet count")
    (List.length out = List.fold_left (fun n s -> n + List.length s) 0 sources);
  assert_monotonic name out;
  let tolerance =
    (List.hd out).tb +. latest (List.map (fun s -> (List.hd s).tb) sources)
  in
  ignore
    (List.fold_left
       (fun (i, previous_end) s ->
         let segment = slice i (List.length s) out in
         List.iter
           (fun (o, p) ->
             check
               (name ^ ": PTS - DTS kept")
               (Float.abs (pts o -. dts o -. (pts p -. dts p)) <= tolerance))
           (zip_short segment s);
         Option.iter
           (fun e ->
             check
               (name ^ ": join after previous end")
               (dts (List.hd segment) >= e -. tolerance))
           previous_end;
         (i + List.length s, Some (dts (last segment) +. duration (last s))))
       (0, None) sources);
  let span =
    dts (last out) +. duration (last (last sources)) -. dts (List.hd out)
  in
  let expected =
    List.fold_left
      (fun e s -> e +. end_of (last s) -. dts (List.hd s))
      0. sources
  in
  check (name ^ ": total duration") (Float.abs (span -. expected) <= 1. /. 30.)

let kc1 () =
  require ["bf1.mp4"; "bf2.mp4"; "bf3.mp4"];
  let inputs = ["bf1.mp4"; "bf2.mp4"; "bf3.mp4"] in
  let sources = List.map (fun f -> packets f `Video) inputs in
  List.iter
    (fun (format, ext) ->
      let out = "kc1." ^ ext in
      ignore
        (run_ok ("kc1_" ^ ext) (copy_script inputs "%video.copy" out format));
      kc1_assertions ("kc1 " ^ format) (packets out `Video) sources)
    [("mp4", "mp4"); ("mpegts", "ts")];
  List.iter
    (fun scenario ->
      let out = Printf.sprintf "kc1_%s.ts" scenario in
      produce scenario "mpegts" out inputs;
      kc1_assertions ("kc1 " ^ scenario) (packets out `Video) sources)
    ["short"; "nodur"]

(* known-complexity.md §2: one offset per source, aligned on the longest. *)
let kc2 () =
  require ["avoff.mp4"; "gop.mp4"; "midgop.mp4"];
  let joins = 10 in
  let src = packets "avoff.mp4" in
  let counts =
    [(`Audio, List.length (src `Audio)); (`Video, List.length (src `Video))]
  in
  let n kind = List.assoc kind counts in
  let expected = pts (List.hd (src `Audio)) -. pts (List.hd (src `Video)) in
  let results =
    List.mapi
      (fun i order ->
        let out = Printf.sprintf "kc2_%d.ts" i in
        ignore
          (run_ok
             (Printf.sprintf "kc2_%d" i)
             (copy_script (repeat (joins + 1) "avoff.mp4") order out "mpegts"));
        let pkts = packets out in
        List.iter
          (fun (kind, n) ->
            check "kc2: count" (List.length (pkts kind) = n * (joins + 1));
            assert_monotonic "kc2" (pkts kind);
            let out_tb = (List.hd (pkts kind)).tb in
            for j = 0 to joins do
              List.iter
                (fun (a, b) ->
                  check "kc2: steps kept within a source"
                    (Float.abs (a -. b) <= out_tb +. (List.hd (src kind)).tb))
                (zip_short
                   (steps (slice (j * n) n (pkts kind)))
                   (steps (src kind)))
            done)
          counts;
        let start kind = pts (List.hd (drop (joins * n kind) (pkts kind))) in
        check "kc2: A/V start relation kept after the last join"
          (Float.abs (start `Audio -. start `Video -. expected) <= 1. /. 30.);
        pkts)
      both_orders
  in
  List.iter
    (fun kind ->
      check "kc2: identical in both declaration orders"
        (timestamps ((List.nth results 0) kind)
        = timestamps ((List.nth results 1) kind)))
    [`Audio; `Video];
  produce "compressed" "mpegts" "kc2_compressed.ts"
    (repeat (joins + 1) "avoff.mp4");
  let pkts = packets "kc2_compressed.ts" in
  List.iter
    (fun (kind, n) ->
      assert_monotonic "kc2 compressed" (pkts kind);
      check "kc2 compressed: count" (List.length (pkts kind) = n * (joins + 1)))
    counts;
  for j = 1 to joins do
    let new_a = List.nth (pkts `Audio) (j * n `Audio)
    and new_v = List.nth (pkts `Video) (j * n `Video) in
    let ends =
      List.map
        (fun (kind, n) ->
          dts (List.nth (pkts kind) ((j * n) - 1)) +. duration (last (src kind)))
        counts
    in
    check "kc2 compressed: no first packet before the latest previous end"
      (min (dts new_a) (dts new_v) >= latest ends -. 0.001);
    check "kc2 compressed: A/V relation kept at each join"
      (Float.abs (pts new_a -. pts new_v -. expected) <= 0.01)
  done;
  let first = packets "gop.mp4" and second = packets "midgop.mp4" in
  let key = List.find (fun p -> p.key) (second `Video) in
  let first_audio = List.find (fun p -> pts p >= pts key) (second `Audio) in
  let relation = pts first_audio -. pts key in
  let outputs =
    List.mapi
      (fun i order ->
        let out = Printf.sprintf "kc2b_%d.ts" i in
        ignore
          (run_ok
             (Printf.sprintf "kc2b_%d" i)
             (copy_script ["gop.mp4"; "midgop.mp4"] order out "mpegts"));
        let pkts = packets out in
        let n kind = List.length (first kind) in
        let ends =
          List.map
            (fun kind -> copied_end ~src:(first kind) (pkts kind))
            [`Audio; `Video]
        in
        let new_a = List.nth (pkts `Audio) (n `Audio)
        and new_v = List.nth (pkts `Video) (n `Video) in
        check "kc2: no first packet before the latest previous end"
          (min (dts new_a) (dts new_v) >= latest ends -. 0.001);
        check "kc2: new source A/V relation kept"
          (Float.abs (pts new_a -. pts new_v -. relation) <= 1. /. 30.);
        pkts)
      both_orders
  in
  List.iter
    (fun kind ->
      check "kc2: identical in both orders at a mid-GOP join"
        (timestamps ((List.nth outputs 0) kind)
        = timestamps ((List.nth outputs 1) kind)))
    [`Audio; `Video]

(* known-complexity.md §3: switching copied streams on keyframes. *)
let kc3 () =
  require ["gop.mp4"; "midgop.mp4"; "nokey.mp4"; "latekey.mp4"];
  let first = packets "gop.mp4" and second = packets "midgop.mp4" in
  let n kind = List.length (first kind) in
  ignore
    (run_ok "kc3"
       (copy_script ["gop.mp4"; "midgop.mp4"] "%audio.copy, %video.copy"
          "kc3.ts" "mpegts"));
  let pkts = packets "kc3.ts" in
  assert_monotonic "kc3 audio" (pkts `Audio);
  assert_monotonic "kc3 video" (pkts `Video);
  let new_v = drop (n `Video) (pkts `Video)
  and new_a = drop (n `Audio) (pkts `Audio) in
  check "kc3: new source has video" (new_v <> []);
  check "kc3: new source has audio" (new_a <> []);
  check "kc3: first video packet of the new source is a keyframe"
    (List.hd new_v).key;
  let dropped = first_index (fun p -> p.key) (second `Video) in
  check "kc3: video packets before the keyframe dropped"
    (List.length new_v = List.length (second `Video) - dropped);
  let ends =
    List.map
      (fun kind -> copied_end ~src:(first kind) (pkts kind))
      [`Video; `Audio]
  in
  let gap = min (dts (List.hd new_v)) (dts (List.hd new_a)) -. latest ends in
  check "kc3: no gap at a whole-output switch" (gap >= 0. && gap <= 1. /. 120.);
  check "kc3: audio of the new source starts at its keyframe"
    (pts (List.hd new_a) >= pts (List.hd new_v) -. (1024. /. 44100.));
  ignore
    (run_ok "kc3a"
       (copy_script ["gop.mp4"; "midgop.mp4"] "%audio.copy" "kc3a.ts" "mpegts"));
  check "kc3: audio-only copy discards nothing"
    (List.length (packets "kc3a.ts" `Audio)
    = n `Audio + List.length (second `Audio));
  ignore
    (run_ok "kc3_mux"
       {|a = once(single("gop.mp4"))
v = once(single("midgop.mp4"))
let {audio} = source.tracks(a)
let {video} = source.tracks(v)
s = source({audio=audio, video=video})
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %ffmpeg(format="mpegts", %audio.copy, %video.copy), "kc3_mux.ts", s)
o.on_stop(synchronous=true, shutdown)
|});
  let pkts = packets "kc3_mux.ts" in
  check "kc3: different sources, both streams present"
    (pkts `Audio <> [] && pkts `Video <> []);
  check "kc3: different sources, video starts on a keyframe"
    (List.hd (pkts `Video)).key;
  check "kc3: different sources, audio not delayed by the video's wait"
    (dts (List.hd (pkts `Audio)) < dts (List.hd (pkts `Video)) -. 0.3);
  ignore
    (run_ok "kc3_nokey"
       (copy_script ["nokey.mp4"; "gop.mp4"] "%audio.copy, %video.copy"
          "kc3_nokey.ts" "mpegts"));
  let pkts = packets "kc3_nokey.ts" in
  check "kc3: the output resumes on the next source"
    (List.length (pkts `Video) = n `Video && (List.hd (pkts `Video)).key);
  let late = packets "latekey.mp4" in
  ignore
    (run_ok "kc3_latekey"
       (copy_script ["latekey.mp4"] "%audio.copy, %video.copy" "kc3_latekey.ts"
          "mpegts"));
  let pkts = packets "kc3_latekey.ts" in
  check "kc3: audio starts at the wait bound"
    (List.length (pkts `Audio) >= List.length (late `Audio) * 9 / 10);
  check "kc3: video starts later on its own keyframe"
    (pkts `Video <> [] && (List.hd (pkts `Video)).key);
  produce "continuing" "mpegts" "kc3_continuing.ts" ["gop.mp4"];
  let pkts = packets "kc3_continuing.ts" in
  check "kc3: no packet discarded at a continuing switch"
    (List.length (pkts `Video) = n `Video
    && List.length (pkts `Audio) = n `Audio)

(* The media segments of an HLS playlist, each with the init segment that
   precedes it, and the count of discontinuities. *)
let hls_segments dir playlist =
  let lines =
    List.map String.trim
      (String.split_on_char '\n'
         (In_channel.with_open_bin
            (Filename.concat dir playlist)
            In_channel.input_all))
  in
  let map_prefix = {|#EXT-X-MAP:URI="|} in
  let _, segments, discontinuities =
    List.fold_left
      (fun (init, segments, discontinuities) line ->
        if String.starts_with ~prefix:map_prefix line then (
          let n = String.length map_prefix in
          ( Some (String.sub line n (String.length line - n - 1)),
            segments,
            discontinuities ))
        else if line = "#EXT-X-DISCONTINUITY" then
          (init, segments, discontinuities + 1)
        else if line <> "" && line.[0] <> '#' then
          (init, (Option.get init, line) :: segments, discontinuities)
        else (init, segments, discontinuities))
      (None, [], 0) lines
  in
  (List.rev segments, discontinuities)

let concat_files out files =
  Out_channel.with_open_bin out (fun oc ->
      List.iter
        (fun f ->
          output_string oc (In_channel.with_open_bin f In_channel.input_all))
        files)

let ogg_links path =
  let data = In_channel.with_open_bin path In_channel.input_all in
  let rec count i n =
    match String.index_from_opt data i 'O' with
      | Some j when j + 6 <= String.length data ->
          if String.sub data j 4 = "OggS" then
            count (j + 4)
              (if Char.code data.[j + 5] land 2 <> 0 then n + 1 else n)
          else count (j + 1) n
      | _ -> n
  in
  count 0 0

(* known-complexity.md §4: lifetime of the per-source state. Live words are
   read after a compaction, so the bound is on retained state, not on heap
   slack. *)
let kc4 () =
  require ["gop.mp4"; "midgop.mp4"; "avoff.mp4"];
  let log =
    run_ok "kc4_memory"
      {|settings.decoder.decoders.set(["ffmpeg"])
s = single("short.mp4")
clock.assign_new(sync="none", [s])
plays = ref(0)
marks = ref([])
s.on_track(synchronous=true, fun (_) -> begin
  plays := plays() + 1
  if plays() == 100 or plays() == 1000 then
    runtime.gc.compact()
    marks := [...marks(), runtime.gc.stat().live_words]
  end
  if plays() == 1000 then
    print("LIVE #{marks()}")
    shutdown()
  end
end)
output.file(fallible=true, %ffmpeg(format="mpegts", %audio.copy, %video.copy), "kc4_memory.ts", s)
|}
  in
  (match
     Scanf.sscanf_opt
       (List.find
          (String.starts_with ~prefix:"LIVE")
          (String.split_on_char '\n' log))
       "LIVE [%d, %d]"
       (fun a b -> (a, b))
   with
    | Some (early, late) ->
        check
          (Printf.sprintf
             "kc4: live words flat over 900 copied sources (%d then %d)" early
             late)
          (abs (late - early) * 200 < early)
    | None -> fail "kc4: no live-word measurement in the log");
  let runs =
    List.map
      (fun (name, reclaim) ->
        let out = name ^ ".ts" in
        ignore
          (run_ok name
             (Printf.sprintf
                {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([once(single("gop.mp4")), once(single("midgop.mp4"))])
s.on_track(synchronous=true, fun (_) -> %s)
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="mpegts", %%audio.copy, %%video.copy), "%s", s)
o.on_stop(synchronous=true, shutdown)
|}
                (if reclaim then "runtime.gc.compact()" else "()")
                out));
        packets out)
      [("kc4_reclaimed", true); ("kc4_plain", false)]
  in
  List.iter
    (fun kind ->
      check "kc4: the output does not depend on when memory is reclaimed"
        (timestamps ((List.nth runs 0) kind)
        = timestamps ((List.nth runs 1) kind)))
    [`Audio; `Video];
  let sources =
    {|s = sequence([once(single("avoff.mp4")), once(single("avoff.mp4")), once(single("avoff.mp4"))])|}
  in
  let av = {|%audio(codec="aac"), %video(codec="libx264")|} in
  ignore
    (run_ok "kc4_inline"
       (Printf.sprintf
          {|settings.decoder.decoders.set(["ffmpeg"])
%s
s = ffmpeg.encode.audio_video(%%ffmpeg(%s), s)
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="mpegts", %%audio.copy, %%video.copy), "kc4_inline.ts", s)
o.on_stop(synchronous=true, shutdown)
|}
          sources av));
  ignore
    (run_ok "kc4_direct"
       (Printf.sprintf
          {|settings.decoder.decoders.set(["ffmpeg"])
%s
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="mpegts", %s), "kc4_direct.ts", s)
o.on_stop(synchronous=true, shutdown)
|}
          sources av));
  let inline = packets "kc4_inline.ts" and direct = packets "kc4_direct.ts" in
  assert_monotonic "kc4 inline audio" (inline `Audio);
  assert_monotonic "kc4 inline video" (inline `Video);
  let relation pkts =
    pts (List.hd (pkts `Audio)) -. pts (List.hd (pkts `Video))
  in
  check "kc4: inline audio and video then copied start as a direct encode"
    (Float.abs (relation inline -. relation direct) < 1e-3);
  check "kc4: inline video then copied keeps every packet"
    (List.length (inline `Video) = List.length (direct `Video));
  (* The source ends with its video track, which cuts the audio encoder's
     tail packet where it reaches past the video's end. *)
  check "kc4: inline audio then copied keeps every packet but the tail"
    (List.length (direct `Audio) - List.length (inline `Audio) <= 1);
  let chains =
    List.map
      (fun (name, reclaim) ->
        let out = name ^ ".ogg" in
        ignore
          (run_ok name
             (Printf.sprintf
                {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([once(single("vq2.ogg")), once(single("vq2.ogg"))])
s.on_track(synchronous=true, fun (_) -> %s)
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="ogg", %%audio.copy), "%s", s)
o.on_stop(synchronous=true, shutdown)
|}
                (if reclaim then "runtime.gc.compact()" else "()")
                out));
        check "kc4: a metadata restart with copied audio chains a link"
          (ogg_links out = 2);
        timestamps (packets out `Audio))
      [("kc4_ogg_reclaimed", true); ("kc4_ogg_plain", false)]
  in
  check "kc4: the Ogg restart does not depend on when memory is reclaimed"
    (List.nth chains 0 = List.nth chains 1);
  let log =
    run_ok "kc4_sleep"
      {|log.level.set(4)
current = ref(true)
e = ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), sine())
f = ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), sine(880.))
d = source.dynamic(track_sensitive=false, fun () -> current() ? e : f)
thread.run(every=1., fun () -> current := not current())
o = output.file(fallible=true, %ffmpeg(format="adts", %audio.copy), "kc4_sleep.aac", d)
thread.run(delay=6.5, shutdown)
|}
  in
  check "kc4: the inline encoders went to sleep and woke up"
    (occurrences "Source ffmpeg_encode_audio gets down" log >= 2
    && occurrences "Source ffmpeg_encode_audio gets up" log >= 3);
  let audio = packets "kc4_sleep.aac" `Audio in
  assert_monotonic "kc4 sleep" audio;
  check "kc4: no packet dropped across sleeps and wake-ups"
    ((not (contains "DTS dropped" log || contains "decreasing DTS" log))
    && List.length audio > 6 * 43);
  produce "exact" "mpegts" "kc4_removed.ts"
    (List.init 20 (fun _ -> "nokey.mp4") @ ["gop.mp4"]);
  let removed = packets "kc4_removed.ts" in
  let gop = packets "gop.mp4" in
  check "kc4: after many streams removed before ready, the next set starts"
    (List.length (removed `Video) = List.length (gop `Video)
    && (List.hd (removed `Video)).key);
  (* The first file's audio ends after its video, so the second file's audio
     offset differs from the one its video alone would take. *)
  produce "audio_switch" "mpegts" "kc4_audio_switch.ts"
    ["avlong.mp4"; "midgop.mp4"; "gop.mp4"];
  let output = packets "kc4_audio_switch.ts" in
  let before = packets "avlong.mp4" in
  let after media = List.nth (output media) (List.length (before media)) in
  let input = packets "midgop.mp4" in
  let key = Option.get (first_key (input `Video)) in
  check "kc4: a late video gets the offset its audio used after the audio left"
    ((after `Video).key
    && near ~within:0.002
         (pts key -. pts (List.hd (input `Audio)))
         (pts (after `Video) -. pts (after `Audio)))

(* known-complexity.md §5: packets with no DTS or a non-increasing DTS. *)
let kc5 () =
  require ["gop.mp4"];
  let plays = 2 in
  let src = packets "gop.mp4" in
  let n_a = List.length (src `Audio) and n_v = List.length (src `Video) in
  let run ?(format = "mpegts") scenario =
    let out = Printf.sprintf "kc5_%s.%s" scenario format in
    produce scenario format out (repeat plays "gop.mp4");
    let pkts = packets out in
    assert_monotonic ("kc5 " ^ scenario) (pkts `Audio);
    assert_monotonic ("kc5 " ^ scenario) (pkts `Video);
    pkts
  in
  let pkts = run "tb_change" in
  check "kc5: a time base change within one source drops no packet"
    (List.length (pkts `Video) = n_v * plays);
  let pkts = run "nodts_audio" in
  check "kc5: audio packets without DTS are written"
    (List.length (pkts `Audio) = n_a * plays);
  (* Size differences, since the container may add a constant header to each
     packet. *)
  let shape l =
    List.map
      (fun (a, b) -> b.size - a.size)
      (pairs (drop (List.length l - 20) l))
  in
  let pkts = run "nodts_video" in
  check "kc5: the output keeps running to the end without video DTS"
    (shape (pkts `Audio) = shape (src `Audio));
  check "kc5: no DTS taken from PTS on a stream with reordering"
    (not (List.exists (fun p -> p.pts = p.dts) (pkts `Video)));
  let pkts = run "equal_dts" in
  check "kc5: an equal DTS drops that packet only"
    (List.length (pkts `Video) = (n_v - 1) * plays);
  let pkts = run "backward_dts" in
  check "kc5: backward DTS drop those packets only"
    (List.length (pkts `Video) = (n_v - 3) * plays);
  let pkts = run "restart_dts" in
  check "kc5: a source whose DTS restarts is carried again"
    (List.length (pkts `Video) >= plays * (30 + 20));
  let pkts = run ~format:"flv" "close_dts" in
  check "kc5: DTS closer than one container unit are written"
    (List.length (pkts `Video) = n_v * plays)

(* known-complexity.md §6: re-encoding across format and track changes. *)
let kc6 () =
  require ["fmtchange.ts"; "besteffort.avi"];
  ignore
    (run_ok "kc6"
       (copy_script ["fmtchange.ts"] {|%video(codec="libx264", g=250)|}
          "kc6.nut" "nut"));
  let times = List.sort compare (List.map pts (packets "kc6.nut" `Video)) in
  check "kc6: every input frame reaches the output across two format changes"
    (List.length times = 6 * 25);
  check "kc6: PTS step by one frame across the changes"
    (List.for_all
       (fun (a, b) -> Float.abs (b -. a -. 0.04) < 1e-6)
       (pairs times));
  ignore
    (run_ok "kc6_besteffort"
       (copy_script ["besteffort.avi"] {|%video(codec="libx264")|}
          "kc6_besteffort.nut" "nut"));
  let times =
    List.sort compare (List.map pts (packets "kc6_besteffort.nut" `Video))
  in
  check "kc6: frames timed by their best-effort timestamp are all kept"
    (List.length times = timed_frames "besteffort.avi");
  check "kc6: best-effort frames are one frame apart"
    (List.for_all
       (fun (a, b) -> Float.abs (b -. a -. 0.04) < 1e-6)
       (pairs times));
  ignore
    (run_ok "kc6_audio"
       (copy_script ["a44.wav"; "a48.wav"] {|%audio(codec="aac")|} "kc6.aac"
          "adts"));
  let n = List.length (packets "kc6.aac" `Audio) in
  check
    "kc6: 44.1 then 48 kHz re-encoded to the input duration within one frame"
    (abs ((n * 1024) - (4 * 44100)) <= 2 * 1024)

(* known-complexity.md §7: a raw stream across a time base change; the
   inputs carry a layout with no channel order, as a WAV file without a
   channel mask decodes. *)
let kc7 () =
  ignore
    (run_ok "kc7"
       (copy_script ["a44.wav"; "a48.wav"] {|%audio.raw(codec="aac")|} "kc7.nut"
          "nut"));
  let audio = packets "kc7.nut" `Audio in
  assert_monotonic "kc7" audio;
  check "kc7: every sample of both sources reaches the output"
    (abs ((List.length audio * 1024) - (4 * 44100)) <= 2 * 1024)

(* known-complexity.md §8: sparse streams, late streams and the muxer's first
   write. *)
let kc8 () =
  require
    [
      "avoff.mp4";
      "latevideo.ts";
      "earlyaudio.ts";
      "sub_short.mkv";
      "sub_late.mkv";
      "sub_early.mkv";
    ];
  Unix.mkdir "kc8_hls" 0o755;
  let log =
    run_ok "kc8_first_write"
      {|audio.samplerate.set(48000)
s = sequence([once(single("avoff.mp4"))])
s = ffmpeg.encode.audio_video(%ffmpeg(%audio(codec="aac", ar=44100), %video(codec="libx264")), s)
clock.assign_new(sync="none", [s])
o = output.file.hls(fallible=true, segment_duration=2., "kc8_hls", [("v", %ffmpeg(format="mp4", movflags="+dash+skip_sidx+skip_trailer+frag_custom", frag_duration=2, %audio.copy, %video.copy))], s)
o.on_stop(synchronous=true, shutdown)
|}
  in
  check "kc8: the earliest packet reaches the muxer first"
    (not (contains "avoid negative timestamp" log));
  let tracks = {|%audio(codec="aac"), %video(codec="libx264")|} in
  let log =
    run_ok "kc8_late" (live_script "latevideo.ts" tracks "kc8_late.nut")
  in
  check "kc8: one warning while the video is awaited"
    (occurrences "not seen within" log = 1);
  (match markers "kc8_late.nut" with
    | [beep], [flash] ->
        check "kc8: a late video and its audio start together"
          (Float.abs (beep -. flash) <= 0.05)
    | beeps, flashes ->
        fail "kc8: %d beeps and %d flashes in place of one each"
          (List.length beeps) (List.length flashes));
  ignore
    (run_ok "kc8_early"
       (live_script "earlyaudio.ts"
          {|%audio(codec="pcm_s16le"), %video(codec="libx264")|} "kc8_early.nut"));
  let pkts = packets "kc8_early.nut" in
  check "kc8: early audio and video start within one audio frame"
    (Float.abs (pts (List.hd (pkts `Audio)) -. pts (List.hd (pkts `Video)))
    <= 1024. /. 44100.);
  List.iter
    (fun (name, files, audio, at) ->
      let out = name ^ ".mkv" in
      ignore
        (run_ok name
           (copy_script files (audio ^ ", %subtitles.copy") out "matroska"));
      match List.map pts (packets out `Subtitle) with
        | [_; cue] ->
            check
              (name ^ ": the late cue keeps its distance from the join")
              (Float.abs (cue -. at) <= 0.1)
        | l -> fail "%s: %d cues in place of 2" name (List.length l))
    [
      ("kc8_join", ["sub_short.mkv"; "sub_late.mkv"], "%audio.copy", 1202.);
      ("kc8_gap", ["sub_early.mkv"; "sub_late.mkv"], "%audio.copy", 1860.);
      ( "kc8_alone",
        ["sub_early.mkv"; "sub_late.mkv"],
        {|%audio(codec="aac")|},
        1860. );
    ]

(* known-complexity.md §10: data held back at the end of a stream, one run per
   path; AAC writes one frame of priming ahead of the samples. *)
let kc10 () =
  require ["count.mp4"; "count.wav"];
  let frames = List.length (packets "count.mp4" `Video) in
  let samples =
    int_of_float
      (Float.round
         (List.fold_left
            (fun t p -> t +. duration p)
            0.
            (packets "count.wav" `Audio)
         *. 44100.))
  in
  let aac_packets = (samples + 1024 + 1023) / 1024 in
  let video = {|s = sequence([once(single("count.mp4"))])|}
  and audio = {|s = sequence([once(single("count.wav"))])|} in
  List.iter
    (fun (name, source, format, tracks, kind, expected) ->
      ignore
        (run_ok name
           (Printf.sprintf
              {|settings.decoder.decoders.set(["ffmpeg"])
%s
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="%s", %s), "%s.out", s)
o.on_stop(synchronous=true, shutdown)
|}
              source format tracks name));
      check
        (Printf.sprintf "%s: %d packets" name expected)
        (List.length (packets (name ^ ".out") kind) = expected))
    [
      ("kc10_copy", video, "nut", "%video.copy", `Video, frames);
      ( "kc10_internal",
        video,
        "nut",
        {|%video(codec="libx264")|},
        `Video,
        frames );
      ("kc10_raw", video, "nut", {|%video.raw(codec="libx264")|}, `Video, frames);
      ( "kc10_reencode",
        audio,
        "adts",
        {|%audio(codec="aac")|},
        `Audio,
        aac_packets );
      ( "kc10_inline",
        audio ^ {|
s = ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), s)|},
        "adts",
        "%audio.copy",
        `Audio,
        aac_packets );
      ( "kc10_filter",
        audio
        ^ {|
def mkfilter(graph) =
  a = ffmpeg.filter.audio.input(graph, source.tracks(s).audio)
  a = ffmpeg.filter.anull(graph, a)
  s = ffmpeg.filter.audio.output(graph, a)
  source({audio=s})
end
s = ffmpeg.filter.create(mkfilter)|},
        "adts",
        {|%audio.raw(codec="aac")|},
        `Audio,
        aac_packets );
    ]

(* known-complexity.md §11: codec parameters across copied sources. *)
let kc11 () =
  let log =
    run_ok "kc11_form"
      (playlist_script "kc11_form"
         ["h264_a.mp4"; "h264_b.ts"; "h264_c.mp4"]
         "%video.copy" "kc11_form.mkv" "matroska")
  in
  check "kc11: a change of bitstream form is rejected at open, naming it"
    (contains "bitstream form differs" log);
  check "kc11: no packet of the rejected file reaches the output"
    (List.length (packets "kc11_form.mkv" `Video) = 100);
  let log =
    run_ok "kc11_codec"
      ("settings.ffmpeg.content.copy.relaxed_compatibility_check.set(true)\n"
      ^ playlist_script "kc11_codec" ["aac.mp4"; "mp3.mp3"] "%audio.copy"
          "kc11_codec.mkv" "matroska")
  in
  check "kc11: a codec change is rejected with the relaxed setting on"
    (contains "codec differs" log);
  let profiles = ["h264_high.mp4"; "h264_main.mp4"] in
  ignore
    (run_ok "kc11_extradata"
       (copy_script profiles "%video.copy" "kc11_extradata.ts" "mpegts"));
  check "kc11: differing extradata keeps an MPEG-TS output streaming"
    (List.length (packets "kc11_extradata.ts" `Video) = 100);
  let _, log =
    run_script "kc11_extradata_mp4"
      (copy_script profiles "%video.copy" "kc11_extradata.mp4" "mp4")
  in
  check "kc11: differing extradata stops an MP4 output at the switch"
    (contains "cannot follow" log
    && List.length (packets "kc11_extradata.mp4" `Video) = 50);
  let log =
    run_ok "kc11_colour"
      (copy_script
         ["c709.mp4"; "c601.mp4"; "c709.mp4"]
         "%video.copy" "kc11_colour.ts" "mpegts")
  in
  check "kc11: files differing in a colour property all play"
    (List.length (packets "kc11_colour.ts" `Video) = 150);
  check "kc11: one warning, at the switch away from the stream header"
    (occurrences "colour space differs (bt709 against bt470bg)" log = 1
    && occurrences "differs" log = 1);
  let log =
    run_ok "kc11_matrix"
      (copy_script ["r0.mp4"; "r90.mp4"] "%video.copy" "kc11_matrix.mkv"
         "matroska")
  in
  check "kc11: files differing in their display matrix all play in Matroska"
    (List.length (packets "kc11_matrix.mkv" `Video) = 100);
  check "kc11: the display matrix difference is warned"
    (contains "Display Matrix differs" log);
  Unix.mkdir "kc11_hls" 0o755;
  Unix.mkdir "kc11_keep" 0o755;
  ignore
    (run_ok "kc11_hls"
       {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([once(single("h264_high.mp4")), once(single("h264_main.mp4"))])
clock.assign_new(sync="none", [s])
o = output.file.hls(fallible=true, segment_duration=1., "kc11_hls", [("v", %ffmpeg(format="mp4", %video.copy))], s)
o.on_file_change(synchronous=true, fun ({state, path}) -> if state != "deleted" then file.copy(force=true, path, "kc11_keep/") end)
o.on_stop(synchronous=true, shutdown)
|});
  let segments, discontinuities = hls_segments "kc11_keep" "v.m3u8" in
  let inits = List.sort_uniq compare (List.map fst segments) in
  check
    (Printf.sprintf
       "kc11: an fMP4 HLS variant restarts after a discontinuity with a new \
        init (%d discontinuities, %d inits, %d segments)"
       discontinuities (List.length inits) (List.length segments))
    (discontinuities = 1 && List.length inits = 2);
  let starts =
    List.map
      (fun (init, segment) ->
        concat_files "kc11_segment.mp4"
          [
            Filename.concat "kc11_keep" init; Filename.concat "kc11_keep" segment;
          ];
        let video = packets "kc11_segment.mp4" `Video in
        check "kc11: every HLS segment starts on a key packet"
          (List.hd video).key;
        (pts (List.hd video), pts (last video)))
      segments
  in
  check "kc11: HLS timestamps rise across the restart"
    (List.for_all (fun ((_, a), (b, _)) -> b > a) (pairs starts));
  let log =
    run_ok "kc11_ogg"
      (copy_script ["vq2.ogg"; "vq8.ogg"] "%audio.copy" "kc11_chain.ogg" "ogg")
  in
  check "kc11: an Ogg file output chains a new link at the switch"
    (contains "restarting the container" log && ogg_links "kc11_chain.ogg" = 2);
  check "kc11: every packet of both sources is in the chained file"
    (List.length (packets "kc11_chain.ogg" `Audio)
    = List.length (packets "vq2.ogg" `Audio)
      + List.length (packets "vq8.ogg" `Audio))

(* known-complexity.md §12: copied video, re-encoded audio, many joins. *)
let kc12 () =
  require ["marked_midgop.mp4"];
  ignore
    (run_ok "kc12"
       (copy_script
          (repeat 50 "marked_midgop.mp4")
          {|%video.copy, %audio(codec="aac")|} "kc12.mkv" "matroska"));
  let beeps, flashes = markers "kc12.mkv" in
  check "kc12: every file's markers present"
    (List.length beeps = 50 && List.length flashes = 50);
  check "kc12: audio and video of every file within one video frame"
    (List.for_all2 (fun b f -> Float.abs (b -. f) <= 0.04) beeps flashes)

(* encoder.md §8.8: segment grid and playlist announcements of an MPEG-TS
   variant, by default over a video with a keyframe every 4 s and 1 s
   segments. *)
let hls_variant ?(file = "gop4.mp4") ?(segment = 1.) name tracks =
  Unix.mkdir name 0o755;
  let keep = name ^ "_keep" in
  Unix.mkdir keep 0o755;
  let log =
    run_ok name
      (Printf.sprintf
         {|settings.decoder.decoders.set(["ffmpeg"])
s = once(single(%S))
clock.assign_new(sync="none", [s])
o = output.file.hls(fallible=true, segment_duration=%g, %S, [("v", %%ffmpeg(format="mpegts", %s))], s)
o.on_file_change(synchronous=true, fun ({state, path}) -> if state != "deleted" then file.copy(force=true, path, %S) end)
o.on_stop(synchronous=true, shutdown)
|}
         file segment name tracks (keep ^ "/"))
  in
  let lines =
    List.map String.trim
      (String.split_on_char '\n'
         (In_channel.with_open_bin
            (Filename.concat keep "v.m3u8")
            In_channel.input_all))
  in
  let value prefix =
    List.filter_map
      (fun line ->
        if String.starts_with ~prefix line then
          Some
            (String.sub line (String.length prefix)
               (String.length line - String.length prefix))
        else None)
      lines
  in
  let target = int_of_string (List.hd (value "#EXT-X-TARGETDURATION:")) in
  let durations =
    List.map
      (fun d -> float_of_string (List.hd (String.split_on_char ',' d)))
      (value "#EXTINF:")
  in
  let segments =
    List.filter (fun line -> line <> "" && line.[0] <> '#') lines
  in
  check
    (Printf.sprintf "hls %s: no EXTINF exceeds the target duration" name)
    (durations <> []
    && List.for_all (fun d -> Float.to_int (Float.round d) <= target) durations
    );
  ( log,
    List.mem "#EXT-X-INDEPENDENT-SEGMENTS" lines,
    List.map (fun f -> packets (Filename.concat keep f) `Video) segments )

let hls () =
  (* A segment holding a split point is cut there before the bound, not at the
     next keyframe ten seconds later. *)
  ignore
    (hls_variant ~file:"irrgop.mp4" ~segment:2. "hls_irregular" "%video.copy");
  let log, independent, segments =
    hls_variant "hls_copy" "%audio.copy, %video.copy"
  in
  check "hls copy: forced cuts are warned" (contains "segment cut at" log);
  check "hls copy: EXT-X-INDEPENDENT-SEGMENTS is not announced"
    (not independent);
  check "hls copy: some segment starts without a key packet"
    (List.exists (fun v -> v <> [] && not (List.hd v).key) segments);
  let log, independent, segments =
    hls_variant "hls_encode"
      {|%audio(codec="aac"), %video(codec="libx264", preset="ultrafast")|}
  in
  check "hls encode: no forced cut" (not (contains "segment cut at" log));
  check "hls encode: EXT-X-INDEPENDENT-SEGMENTS is announced" independent;
  check "hls encode: every segment starts with a key frame"
    (List.length segments >= 10
    && List.for_all (fun v -> v <> [] && (List.hd v).key) segments);
  Unix.mkdir "hls_restart" 0o755;
  Unix.mkdir "hls_restart_keep" 0o755;
  ignore
    (run_ok "hls_restart"
       {|settings.decoder.decoders.set(["ffmpeg"])
v = sequence([once(single("h264_high.mp4")), once(single("h264_main.mp4"))])
s = source({audio = source.tracks(once(sine(duration=4.))).audio, video = source.tracks(v).video})
clock.assign_new(sync="none", [s])
o = output.file.hls(fallible=true, segment_duration=1., "hls_restart", [("v", %ffmpeg(format="mp4", %audio(codec="aac"), %video.copy))], s)
o.on_file_change(synchronous=true, fun ({state, path}) -> if state != "deleted" then file.copy(force=true, path, "hls_restart_keep/") end)
o.on_stop(synchronous=true, shutdown)
|});
  let segments, _ = hls_segments "hls_restart_keep" "v.m3u8" in
  let first_init = fst (List.hd segments) in
  let before, after = List.partition (fun (i, _) -> i = first_init) segments in
  let audio (init, segment) =
    concat_files "hls_restart.mp4"
      [
        Filename.concat "hls_restart_keep" init;
        Filename.concat "hls_restart_keep" segment;
      ];
    packets "hls_restart.mp4" `Audio
  in
  let rec until_discontinuity = function
    | line :: rest when line <> "#EXT-X-DISCONTINUITY" ->
        line :: until_discontinuity rest
    | _ -> []
  in
  let boundary =
    List.fold_left ( +. ) 0.
      (List.filter_map
         (fun line ->
           if String.starts_with ~prefix:"#EXTINF:" line then
             Some (Scanf.sscanf line "#EXTINF:%f," Fun.id)
           else None)
         (until_discontinuity
            (List.map String.trim
               (String.split_on_char '\n'
                  (In_channel.with_open_bin
                     (Filename.concat "hls_restart_keep" "v.m3u8")
                     In_channel.input_all)))))
  in
  let last_before = last (audio (last before))
  and first_after = List.hd (audio (List.hd after)) in
  check
    (Printf.sprintf
       "hls restart: re-encoded audio continues the variant timeline (%.4f at \
        a %.4f boundary)"
       (pts first_after) boundary)
    (after <> []
    && near ~within:0.001 boundary (pts first_after)
    && dts first_after > dts last_before)

(* filters.md §12.3: a graph whose build fails ends its track, the output
   streams from a fallback above it, and the graph is tried again after the
   fault retry delay. *)
let filters () =
  let log =
    run_ok "filters_build"
      {|def mkfilter(graph) =
  a = ffmpeg.filter.audio.input(graph, source.tracks(ffmpeg.raw.encode.audio(%ffmpeg(%audio.raw), sine())).audio)
  v = ffmpeg.filter.video.input(graph, source.tracks(ffmpeg.raw.encode.video(%ffmpeg(%video.raw), video.testsrc.ffmpeg())).video)
  let {audio = ao, video = vo} =
    ffmpeg.filter.parse(graph, description="[a]anull[ao];[v]crop=w=4000:h=4000[vo]", inputs={audio = [("a", a)], video = [("v", v)]}, outputs={audio = ["ao"], video = ["vo"]})
  source({audio = ffmpeg.filter.audio.output(graph, list.assoc("ao", ao)), video = ffmpeg.filter.video.output(graph, list.assoc("vo", vo))})
end
g = ffmpeg.filter.create(mkfilter)
backup = source({audio = source.tracks(sine()).audio, video = source.tracks(video.testsrc()).video})
s = fallback([ffmpeg.raw.decode.audio_video(g), backup.{track_sensitive = {false}}])
o = output.dummy(s)
thread.run(delay=12., {print("output time: #{o.time()}"); shutdown()})
|}
  in
  let failures =
    List.filter_map
      (fun line ->
        if contains "failed: Build failed" line then
          Scanf.sscanf line "%_s %d:%d:%d" (fun h m s ->
              Some ((((h * 60) + m) * 60) + s))
        else None)
      (String.split_on_char '\n' log)
  in
  check
    (Printf.sprintf "filters: the failed build is retried (%d failures)"
       (List.length failures))
    (List.length failures >= 2);
  check "filters: retries wait for the fault retry delay"
    (List.for_all (fun (a, b) -> b - a >= 4) (pairs failures));
  check "filters: the output streams from the fallback"
    (match
       List.find_opt
         (String.starts_with ~prefix:"output time: ")
         (String.split_on_char '\n' log)
     with
      | Some line -> Scanf.sscanf line "output time: %f" (fun t -> t >= 11.)
      | None -> false)

(* The links of a chained Ogg file, each written to [<path>.<n>]. *)
let ogg_split path =
  let data = In_channel.with_open_bin path In_channel.input_all in
  let rec starts i acc =
    match String.index_from_opt data i 'O' with
      | Some j when j + 6 <= String.length data ->
          if String.sub data j 4 = "OggS" && Char.code data.[j + 5] land 2 <> 0
          then starts (j + 4) (j :: acc)
          else starts (j + 1) acc
      | _ -> List.rev acc
  in
  let starts = starts 0 [] in
  List.mapi
    (fun n (a, b) ->
      let part = Printf.sprintf "%s.%d" path n in
      Out_channel.with_open_bin part (fun oc ->
          output_string oc (String.sub data a (b - a)));
      part)
    (List.combine starts (List.tl starts @ [String.length data]))

(* encoder.md §13.4: Ogg restarts through a [Stream] output. *)
let ogg_stream () =
  let switch out output =
    ignore
      (run_ok out
         (Printf.sprintf
            {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([once(single("vq2.ogg")), once(single("vq8.ogg"))])
clock.assign_new(sync="none", [s])
o = %s
o.on_stop(synchronous=true, shutdown)
|}
            output));
    match ogg_split out with
      | [first; second] ->
          check
            (out ^ ": every packet of the new source is in the new link")
            (List.length (packets first `Audio)
             = List.length (packets "vq2.ogg" `Audio)
            && List.length (packets second `Audio)
               = List.length (packets "vq8.ogg" `Audio))
      | links -> fail "%s: %d links" out (List.length links)
  in
  switch "ogg_switch_stream.ogg"
    {|output.external(fallible=true, %ffmpeg(format="ogg", %audio.copy), "cat > ogg_switch_stream.ogg", s)|};
  switch "ogg_switch_file.ogg"
    {|output.file(fallible=true, %ffmpeg(format="ogg", %audio.copy), "ogg_switch_file.ogg", s)|};
  let metadata out output =
    ignore
      (run_ok out
         (Printf.sprintf
            {|settings.decoder.decoders.set(["ffmpeg"])
s = sequence([once(single("annotate:title=\"a\":vq2.ogg")), once(single("annotate:title=\"b\":vq2.ogg"))])
clock.assign_new(sync="none", [s])
o = %s
o.on_stop(synchronous=true, shutdown)
|}
            output));
    let links = ogg_split out in
    check
      (Printf.sprintf "%s: a metadata change chains a link (%d links)" out
         (List.length links))
      (List.length links = 2);
    let spans =
      List.map
        (fun link ->
          let audio = packets link `Audio in
          check (out ^ ": every link decodes on its own") (audio <> []);
          (pts (List.hd audio), pts (last audio)))
        links
    in
    check
      (out ^ ": the links are in order")
      (List.for_all (fun ((_, a), (b, _)) -> b <= a +. 1.) (pairs spans))
  in
  let encoded = {|%ffmpeg(format="ogg", %audio(codec="libvorbis"))|} in
  metadata "ogg_meta_stream.ogg"
    (Printf.sprintf
       {|output.external(fallible=true, %s, "cat > ogg_meta_stream.ogg", s)|}
       encoded);
  metadata "ogg_meta_file.ogg"
    (Printf.sprintf {|output.file(fallible=true, %s, "ogg_meta_file.ogg", s)|}
       encoded);
  (* decoder.md §10.2: FFmpeg reports a link's tags on the stream and on its
     decoded frames. *)
  let titles name source =
    let log =
      run_ok name
        (Printf.sprintf
           {|settings.decoder.decoders.set(["ffmpeg"])
s = %s
s.on_metadata(synchronous=true, fun (m) -> print("<title: #{m["title"]}>"))
clock.assign_new(sync="none", [s])
o = output.dummy(fallible=true, s)
o.on_stop(synchronous=true, shutdown)
|}
           source)
    in
    List.filter_map
      (fun mark ->
        match String.split_on_char ' ' mark with
          | ["title:"; title] when title <> "" -> Some title
          | _ -> None)
      (marks ~prefix:"title: " log)
  in
  List.iter
    (fun (name, source) ->
      let titles = titles name source in
      check
        (Printf.sprintf "%s: each link's title reaches the output once (%s)"
           name
           (String.concat ", " titles))
        (titles = ["a"; "b"]))
    [
      ("ogg_meta_decode_file", {|once(single("ogg_meta_file.ogg"))|});
      ( "ogg_meta_decode_stream",
        {|once(input.ffmpeg(new_track_on_metadata=false, "ogg_meta_file.ogg"))|}
      );
    ]

(* A peer on a loopback port, in a child process: it accepts every connection,
   sends [send] and then stalls, never reading. Returns the port and the
   child's pid. *)
let stalled_peer ~send () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.setsockopt_int socket Unix.SO_RCVBUF 4096;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 8;
  let port =
    match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> fail "stalled peer: no port"
  in
  match Unix.fork () with
    | 0 ->
        let clients = ref [] in
        (try
           while true do
             let client, _ = Unix.accept socket in
             clients := client :: !clients;
             ignore (Unix.write_substring client send 0 (String.length send))
           done
         with _ -> ());
        Unix._exit 0
    | pid ->
        Unix.close socket;
        (port, pid)

let with_stalled_peer ~send f =
  let port, pid = stalled_peer ~send () in
  Fun.protect
    ~finally:(fun () ->
      Unix.kill pid Sys.sigkill;
      ignore (Unix.waitpid [] pid))
    (fun () -> f port)

(* io.md §11: output.url's start/stop methods and restart policy; io.md §3: a
   blocking read or write that outlasts [timeout] on a stalled peer. *)
let io () =
  let log =
    run_ok "url_lifecycle"
      {|o = output.url(start=false, url="url_lifecycle.ts", %ffmpeg(format="mpegts", %audio(codec="aac")), sine())
thread.run(delay=0.5, fun () -> begin print("<io: before start #{o.is_started()}>"); o.start() end)
thread.run(delay=2., fun () -> begin print("<io: after start #{o.is_started()}>"); o.stop() end)
thread.run(delay=3., fun () -> begin print("<io: after stop #{o.is_started()}>"); shutdown() end)
|}
  in
  check "io: is_started follows start and stop"
    (marks ~prefix:"io: " log
    = ["io: before start false"; "io: after start true"; "io: after stop false"]
    );
  check "io: a stopped output.url leaves a complete file"
    (packets "url_lifecycle.ts" `Audio <> []);
  let log =
    run_ok "url_no_restart"
      {|s = sine()
o = output.url(restart_delay=null, url="tcp://127.0.0.1:1", %ffmpeg(format="mpegts", %audio(codec="aac")), s)
o.on_error(synchronous=true, fun (_) -> print("<io: error>"))
o.on_stop(synchronous=true, fun () -> print("<io: stopped>"))
output.file(%ffmpeg(format="mpegts", %audio(codec="aac")), "url_no_restart.ts", s)
thread.run(delay=4., fun () -> begin print("<io: started #{o.is_started()}>"); shutdown() end)
|}
  in
  let lines = marks ~prefix:"io: " log in
  check
    (Printf.sprintf
       "io: with restart_delay=null, a failing output.url stops once (%s)"
       (String.concat "; " lines))
    (lines = ["io: error"; "io: stopped"; "io: started false"]);
  check "io: the other output on the clock keeps streaming"
    (match packets "url_no_restart.ts" `Audio with
      | [] -> false
      | audio -> pts (last audio) -. pts (List.hd audio) >= 3.);
  let stream = In_channel.with_open_bin "stall.ts" In_channel.input_all in
  with_stalled_peer ~send:stream (fun port ->
      let log =
        run_ok "input_stall"
          (Printf.sprintf
             {|connections = ref(0)
def on_connect() =
  ref.incr(connections)
  if connections() == 2 then
    print("<io: reconnected>")
    shutdown()
  end
end
s = input.ffmpeg(timeout=1., on_connect=on_connect, format="mpegts", "tcp://127.0.0.1:%d")
output.dummy(fallible=true, s)
thread.run(delay=20., fun () -> begin print("<io: no reconnect>"); shutdown() end)
|}
             port)
      in
      check "io: input.ffmpeg reconnects after a read stalls past its timeout"
        (marks ~prefix:"io: " log = ["io: reconnected"]));
  with_stalled_peer ~send:"" (fun port ->
      let log =
        run_ok "output_stall"
          (Printf.sprintf
             {|s = sine()
clock.assign_new(sync="none", [s])
o = output.url(timeout=1., restart_delay=0.5, url="tcp://127.0.0.1:%d", %%ffmpeg(format="wav", %%audio(codec="pcm_s16le")), s)
o.on_error(synchronous=true, fun (_) -> begin print("<io: write timed out>"); shutdown() end)
thread.run(delay=20., fun () -> begin print("<io: no timeout>"); shutdown() end)
|}
             port)
      in
      check "io: output.url gives up a write that stalls past its timeout"
        (match marks ~prefix:"io: " log with
          | "io: write timed out" :: _ -> true
          | _ -> false));
  with_stalled_peer ~send:"" (fun port ->
      let log =
        run_ok "output_stall_shutdown"
          (Printf.sprintf
             {|s = sine()
clock.assign_new(sync="none", [s])
o = output.url(timeout=1000., url="tcp://127.0.0.1:%d", %%ffmpeg(format="wav", %%audio(codec="pcm_s16le")), s)
o.on_error(synchronous=true, fun (_) -> print("<io: error>"))
thread.run(delay=3., shutdown)
|}
             port)
      in
      check "io: a shutdown ends a stalled write without an error"
        (marks ~prefix:"io: " log = []))

let index_of sub s =
  let length = String.length sub in
  let rec find from =
    if from + length > String.length s then None
    else if String.sub s from length = sub then Some from
    else find (from + 1)
  in
  find 0

let free_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> fail "no free port"
  in
  Unix.close socket;
  port

(* A listener in a child process: [after] seconds after [port] first accepts
   a connection, it fetches [/<mount>] for [seconds] and writes the body to
   [path]. *)
let listen_later ~port ~mount ~after ~seconds path =
  match Unix.fork () with
    | 0 ->
        let body =
          try
            let address = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
            let rec wait_for_server tries =
              let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
              match Unix.connect socket address with
                | () -> Unix.close socket
                | exception Unix.Unix_error _ when tries > 0 ->
                    Unix.close socket;
                    Unix.sleepf 0.2;
                    wait_for_server (tries - 1)
            in
            wait_for_server 300;
            Unix.sleepf after;
            let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
            Unix.connect socket address;
            let request = Printf.sprintf "GET /%s HTTP/1.0\r\n\r\n" mount in
            ignore
              (Unix.write_substring socket request 0 (String.length request));
            Unix.setsockopt_float socket Unix.SO_RCVTIMEO 1.;
            let received = Buffer.create 65536 and chunk = Bytes.create 4096 in
            let deadline = Unix.gettimeofday () +. seconds in
            (try
               while Unix.gettimeofday () < deadline do
                 match Unix.read socket chunk 0 4096 with
                   | 0 -> raise Exit
                   | n -> Buffer.add_subbytes received chunk 0 n
               done
             with _ -> ());
            let data = Buffer.contents received in
            match index_of "\r\n\r\n" data with
              | Some i -> String.sub data (i + 4) (String.length data - i - 4)
              | None -> ""
          with _ -> ""
        in
        Out_channel.with_open_bin path (fun oc -> output_string oc body);
        Unix._exit 0
    | pid -> pid

(* encoder.md §10: a listener joining after the burst gets the stream's header
   first. *)
let harbor () =
  let port = free_port () in
  let listener =
    listen_later ~port ~mount:"late" ~after:12. ~seconds:3. "harbor_late.ogg"
  in
  ignore
    (run_ok "harbor_late"
       (Printf.sprintf
          {|output.harbor(%%ffmpeg(format="ogg", %%audio(codec="libopus", samplerate=48000)), port=%d, mount="late", sine())
thread.run(delay=30., shutdown)
|}
          port));
  ignore (Unix.waitpid [] listener);
  let late = In_channel.with_open_bin "harbor_late.ogg" In_channel.input_all in
  check "harbor: a late listener gets the stream's OpusHead first"
    (String.starts_with ~prefix:"OggS" late
    && match index_of "OpusHead" late with Some i -> i < 100 | None -> false);
  check "harbor: the late listener's stream decodes"
    (packets "harbor_late.ogg" `Audio <> [])

(* decoder.md §6: the FFmpeg decoder produces each pcm content kind. *)
let pcm_kinds () =
  List.iter
    (fun kind ->
      let log =
        run_ok ("pcm_kind_" ^ kind)
          (Printf.sprintf
             {|settings.decoder.decoders.set(["ffmpeg"])
s = once((single("pcm_src.mp3") : source(audio=%s(stereo))))
clock.assign_new(sync="none", [s])
o = output.dummy(fallible=true, s)
o.on_stop(synchronous=true, fun () -> begin print("<decoded: #{s.time()}>"); shutdown() end)
|}
             kind)
      in
      check
        (Printf.sprintf "pcm_kinds: %s decodes the whole file" kind)
        (match marks ~prefix:"decoded: " log with
          | [line] ->
              Scanf.sscanf line "decoded: %f" (fun t ->
                  Float.abs (t -. 3.) < 0.1)
          | _ -> false))
    ["pcm"; "pcm_s16"; "pcm_f32"]

let checks =
  [
    ("kc1", kc1);
    ("kc2", kc2);
    ("kc3", kc3);
    ("kc4", kc4);
    ("kc5", kc5);
    ("kc6", kc6);
    ("kc7", kc7);
    ("kc8", kc8);
    ("kc10", kc10);
    ("kc11", kc11);
    ("kc12", kc12);
    ("hls", hls);
    ("filters", filters);
    ("ogg_stream", ogg_stream);
    ("io", io);
    ("pcm_kinds", pcm_kinds);
    ("harbor", harbor);
  ]

(* The known-complexity checks of spec/tests.md §23 that need media, run by
   the rules of the dune file next to this one as
   [conformance.exe <copy_producer.exe> <check>], in a directory holding the
   fixtures, with [liquidsoap] on the path.

   A check runs scripts or the synthetic copy producer, reads the packets of
   the outputs with the bindings, and passes when at least one assertion ran
   and none failed. *)
let () =
  match Sys.argv with
    | [| _; copy_producer; name |] -> (
        producer := copy_producer;
        match List.assoc name checks () with
          | () when !assertions = 0 ->
              Printf.eprintf "FAIL %s: no assertion ran\n" name;
              exit 1
          | () -> Printf.printf "pass %s (%d assertions)\n" name !assertions
          | exception Check_failed e ->
              Printf.eprintf "FAIL %s: %s\n" name e;
              exit 1)
    | _ ->
        prerr_endline "Usage: conformance.exe <copy_producer.exe> <check>";
        exit 2
