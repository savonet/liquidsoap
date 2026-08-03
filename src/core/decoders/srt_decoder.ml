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

(** Decode SRT files. *)

let log = Log.make ["decoder"; "srt"]

let srt_priorities =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "srt")
    "Priority for the SRT decoder" ~d:1

let srt_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "srt")
    "Mime-types used for guessing SRT format" ~d:["application/x-subrip"]

let srt_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "srt")
    "File extensions used for guessing SRT format" ~d:["srt"]

let () =
  Plug.register Decoder.decoders "srt" ~doc:"Decode srt files."
    {
      Decoder.priority = (fun () -> srt_priorities#get);
      file_extensions = (fun () -> Some srt_file_extensions#get);
      mime_types = (fun () -> Some srt_mime_types#get);
      file_type =
        (fun ~metadata:_ ~ctype:_ fname ->
          if Srt_parser.check_file fname then
            Some
              (Frame.Fields.add Frame.Fields.subtitles Subtitle_content.format
                 (Frame.Fields.make ()))
          else None);
      file_decoder =
        Some
          (fun ~metadata:_ ~ctype fname ->
            let srt = Srt_parser.parse_file fname in
            (* Track the end time of the last subtitle *)
            let file_end_time =
              List.fold_left
                (fun acc ((_, t2), _) ->
                  max acc
                    (Frame.main_of_seconds (Srt_parser.seconds_of_time t2)))
                0 srt
            in
            let srt =
              List.map
                (fun ((t1, t2), s) ->
                  let pos =
                    Frame.main_of_seconds (Srt_parser.seconds_of_time t1)
                  in
                  let end_ticks =
                    Frame.main_of_seconds (Srt_parser.seconds_of_time t2)
                  in
                  let subtitle : Subtitle_content.subtitle =
                    {
                      start_time = 0;
                      end_time = end_ticks - pos;
                      text = s;
                      format = `Text;
                      forced = false;
                    }
                  in
                  (pos, subtitle))
                srt
            in
            let srt = List.to_seq srt |> Queue.of_seq in
            let t = ref 0 in
            let remaining _ = -1 in
            let fread length =
              (* File ends when we've reached the end time of the last subtitle *)
              if !t >= file_end_time then Frame.create ~length:0 ctype
              else (
                let rec fill acc =
                  if Queue.is_empty srt then acc
                  else (
                    let sub_t, sub = Queue.peek srt in
                    let r = sub_t - !t in
                    assert (r >= 0);
                    if r < length then (
                      ignore (Queue.take srt);
                      fill ((r, sub) :: acc))
                    else acc)
                in
                let subtitles = List.rev (fill []) in
                let frame = Frame.create ~length ctype in
                let frame =
                  if subtitles <> [] then (
                    let data = Subtitle_content.lift_data ~length subtitles in
                    Frame.set frame Frame.Fields.subtitles data)
                  else frame
                in
                t := !t + length;
                frame)
            in
            Decoder.
              {
                fread;
                remaining;
                fseek = (fun _ -> 0);
                fclose = (fun () -> ());
              });
      stream_decoder = None;
    }
