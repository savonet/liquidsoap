(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

open Stdlib

module Generator = Generator.From_audio_video_plus
module Generated = Generated.From_audio_video_plus

(* {1 External Input handling} *)

exception Finished of string*bool

class external_input ~kind ~restart ~bufferize ~channels
                     ~restart_on_error ~max
                     ~samplerate command =
  let abg_max_len = Frame.audio_of_seconds max in
  let in_freq = float samplerate in
  let converter =
    Rutils.create_from_iff ~format:`Wav ~channels ~samplesize:16
                           ~audio_src_rate:in_freq
  in
  (* We need a temporary log until the source has an id *)
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
  let log_error = ref (fun _ -> ()) in
  let abg = Generator.create ~log ~kind `Audio in
  let on_stdout in_chan =
    let s = Process_handler.read 1024 in_chan in
    let data = converter s in
    let len = Array.length data.(0) in
    let buffered = Generator.length abg in
    Generator.put_audio abg data 0 (Array.length data.(0));
    if abg_max_len < buffered+len then
      `Delay (Frame.seconds_of_audio (buffered+len-abg_max_len))
    else
      `Continue
  in
  let on_stderr in_chan =
    (!log_error) (Process_handler.read 1024 in_chan);
    `Continue
  in
  let on_stop = function
    | Some _ -> restart_on_error
    | None -> restart
  in
object (self)
  inherit Source.source ~name:"input.external" kind
  inherit Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable process = None

  method stype = Source.Fallible

  method wake_up _ =
    (* Now we can create the log function *)
    log_ref := self#log#f 3 "%s";
    log_error := self#log#f 5 "%s";
    process <- Some (Process_handler.run ~on_stop ~on_stdout 
                                         ~on_stderr ~log command)

  method sleep =
    match process with
      | Some h ->
          Process_handler.kill h;
          process <- None
      | None -> ()
end

let () =
    Lang.add_operator "input.external"
      ~category:Lang.Input
      ~descr:"Stream data from an external application."
      [
        "buffer", Lang.float_t, Some (Lang.float 2.),
         Some "Duration of the pre-buffered data." ;

        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data.";

        "channels", Lang.int_t, Some (Lang.int 2),
        Some "Number of channels.";

        "samplerate", Lang.int_t, Some (Lang.int 44100),
        Some "Samplerate.";

        "restart", Lang.bool_t, Some (Lang.bool true),
        Some "Restart process when exited.";

        "restart_on_error", Lang.bool_t, Some (Lang.bool false),
        Some "Restart process when exited with error.";

        "", Lang.string_t, None,
        Some "Command to execute." ]
      ~kind:Lang.audio_any
      (fun p kind ->
         let command = Lang.to_string (List.assoc "" p) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let channels = Lang.to_int (List.assoc "channels" p) in
         if not (Frame.mul_eq_int kind.Frame.audio channels) then
           raise (Lang.Invalid_value
                   (List.assoc "channels" p,
                    "Incompatible number of channels, \
                     please use a conversion operator.")) ;
         let samplerate = Lang.to_int (List.assoc "samplerate" p) in
         let restart = Lang.to_bool (List.assoc "restart" p) in
         let restart_on_error =
           Lang.to_bool (List.assoc "restart_on_error" p)
         in
         let max = Lang.to_float (List.assoc "max" p) in
          ((new external_input ~kind ~restart ~bufferize ~channels
                               ~restart_on_error ~max
                               ~samplerate command):>Source.source))




(* Video-only input. Ideally this should be merged with previous one and we
   should add support for audio in AVI files. *)

module Img = Image.RGBA32
      
class video ~kind ~restart ~bufferize ~restart_on_error ~max ~create ~get_data =
  let abg_max_len = Frame.master_of_seconds max in
  (* We need a temporary log until the source has an id *)
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
  (* TODO: generators with 0 audio channels should work in `Both mode *)
  let abg = Generator.create ~log ~kind (if kind.Frame.audio = Frame.Zero then `Video else `Both) in
  let priority = Tutils.Non_blocking in
object (self)
  inherit Source.source ~name:"input.external.video" kind
  inherit Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable should_stop = false

  method stype = Source.Fallible

  method wake_up _ =
    (* Now we can create the log function *)
    log_ref := self#log#f 3 "%s" ;
    self#log#f 2 "Starting process.";
    let (_, in_d) as x = create () in
    let rec process ((in_e,in_d) as x) l =
      let do_restart s restart f =
        self#log#f 2 "%s" s;
        begin
          try
            ignore (Unix.close_process_in in_e);
          with
          | _ -> ()
        end;
        if restart then
          begin
            self#log#f 2 "Restarting process.";
            let ((_,in_d) as x) = create () in
            [{ Duppy.Task.
               priority = priority;
               events   = [`Read in_d];
               handler  = process x
             }]
          end
        else
          begin
            f ();
            self#log#f 2 "Task exited.";
            []
          end
      in
      try
        let events =
          if should_stop then raise (Finished ("Source stoped: closing process.",false));
          let len = Generator.length abg - abg_max_len in
          if len >= 0 then
            let delay = Frame.seconds_of_audio len in
            [`Delay delay]
          else
            begin
              if List.mem (`Read in_d) l then
                begin
                  try
                    get_data in_d abg
                  with
                  | End_of_file -> raise (Finished ("Process exited.", restart))
                end;
              [`Read in_d]
            end
        in
        [{ Duppy.Task.
           priority = priority;
           events   = events;
           handler  = process x
         }]
      with
      | Finished (s,b) -> do_restart s b (fun () -> ())
      | e ->
         do_restart
           (Printf.sprintf "Process exited with error: %s" (Printexc.to_string e)) restart_on_error
           (fun () -> raise e)
    in
    let task =
      { Duppy.Task.
        priority = priority;
        events   = [`Read in_d];
        handler  = process x
      }
    in
    Duppy.Task.add Tutils.scheduler task

  method sleep = should_stop <- true
end

(* TODO: factorize some code with input.external.rawvideo *)

(***** AVI *****)
  
let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_video_any in
  let kind = Lang.Unconstrained k in
  Lang.add_operator "input.external.avi"
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Stream data from an external application."
    [
      "buffer", Lang.float_t, Some (Lang.float 1.),
      Some "Duration of the pre-buffered data." ;

      "max", Lang.float_t, Some (Lang.float 10.),
      Some "Maximum duration of the buffered data.";

      "restart", Lang.bool_t, Some (Lang.bool true),
      Some "Restart process when exited.";

      "restart_on_error", Lang.bool_t, Some (Lang.bool false),
      Some "Restart process when exited with error.";

      "", Lang.string_t, None,
      Some "Command to execute."
    ]
    ~kind
    (fun p kind ->
      let command = Lang.to_string (List.assoc "" p) in
      let width = Lazy.force Frame.video_width in
      let height = Lazy.force Frame.video_height in
      let audio_converter = ref None in
      let create () =
        let in_e = Unix.open_process_in command in
        let f = Unix.descr_of_in_channel in_e in
        let h, _ = Avi.Read.headers_simple f in
        let check = function
          | `Video (w,h,fps) ->
             if w <> width then failwith "Wrong video width.";
            if h <> height then failwith "Wrong video height.";
            if fps <> float (Lazy.force Frame.video_rate) then failwith "Wrong video rate."
          | `Audio (channels, audio_src_rate) ->
             let audio_src_rate = float audio_src_rate in
             if !audio_converter <> None then failwith "Only one audio track is supported for now.";
             audio_converter := Some (Rutils.create_from_iff ~format:`Wav ~channels ~samplesize:16 ~audio_src_rate)
        in
        List.iter check h;
        in_e, f
      in
      let get_data fd abg =
        match Avi.Read.chunk fd with
        | `Frame (_, _, data) when Bytes.length data = 0 -> ()
        | `Frame (`Video, _, data) ->
           if Bytes.length data <> width * height * 3 then
             failwith (Printf.sprintf "Wrong video frame size (%d instead of %d)" (Bytes.length data) (width * height * 3));
          let data = Img.of_RGB24_string data width in
          (* Img.swap_rb data; *)
          (* Img.Effect.flip data; *)
          Generator.put_video abg [|[|data|]|] 0 1
        | `Frame (`Audio, _, data) ->
           let converter = Utils.get_some !audio_converter in
           let data = converter data in
           Generator.put_audio abg data 0 (Array.length data.(0))
        | _ -> failwith "Invalid chunk."
      in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      ((new video ~kind ~restart ~bufferize ~restart_on_error ~max ~create ~get_data):>Source.source))

(***** raw video *****)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.video_only in
  let kind = Lang.Unconstrained k in
  Lang.add_operator "input.external.rawvideo"
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Stream data from an external application."
    [
      "buffer", Lang.float_t, Some (Lang.float 1.),
      Some "Duration of the pre-buffered data." ;

      "max", Lang.float_t, Some (Lang.float 10.),
      Some "Maximum duration of the buffered data.";

      "restart", Lang.bool_t, Some (Lang.bool true),
      Some "Restart process when exited.";

      "restart_on_error", Lang.bool_t, Some (Lang.bool false),
      Some "Restart process when exited with error.";

      "", Lang.string_t, None,
      Some "Command to execute."
    ]
    ~kind
    (fun p kind ->
      let command = Lang.to_string (List.assoc "" p) in
      let width = Lazy.force Frame.video_width in
      let height = Lazy.force Frame.video_height in
      let create () =
        let in_e = Unix.open_process_in command in
        let f = Unix.descr_of_in_channel in_e in
        in_e, f
      in
      let buflen = width * height * 3 in
      let buf = Bytes.create buflen in
      let get_data fd abg =
        let r = Unix.read_retry fd buf 0 buflen in
        if r > 0 then
          (
            if r <> buflen then failwith (Printf.sprintf "Wrong video frame size (%d instead of %d)." r buflen);
            let data = Img.of_RGB24_string buf width in
            (* Img.swap_rb data; *)
            (* Img.Effect.flip data; *)
            Generator.put_video abg [|[|data|]|] 0 1
          )
      in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      ((new video ~kind ~restart ~bufferize ~restart_on_error ~max ~create ~get_data):>Source.source))
