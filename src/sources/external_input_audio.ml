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

open Extralib

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
    let data = converter (Bytes.unsafe_to_string s) in
    let len = Array.length data.(0) in
    let buffered = Generator.length abg in
    Generator.put_audio abg data 0 (Array.length data.(0));
    if abg_max_len < buffered+len then
      `Delay (Frame.seconds_of_audio (buffered+len-3*abg_max_len/4))
    else
      `Continue
  in
  let on_stderr in_chan =
    (!log_error) (Bytes.unsafe_to_string (Process_handler.read 1024 in_chan));
    `Continue
  in
  let on_stop = function
    | `Status (Unix.WEXITED 0) -> restart
    | _ -> restart_on_error
  in
object (self)
  inherit Source.source ~name:"input.external.audio" kind
  inherit Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable process = None

  method stype = Source.Fallible

  method wake_up _ =
    (* Now we can create the log function *)
    log_ref := self#log#important "%s";
    log_error := self#log#info "%s";
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
    Lang.add_operator "input.external.audio"
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
           raise (Lang_errors.Invalid_value
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

