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

(** Protocol plugin for speech synthesis *)

open Dtools
open Unix

let conf =
  Conf.void ~p:(Configure.conf#plug "say")
    "Parameters for the say protocol."
let conf_program =
  Conf.string ~p:(conf#plug "program") ~d:Configure.tts_program
    "Program for syntesizing voices (takes as argument the text, \
     the file to synthesize to, and optionnaly the voice to use)."

let dlog = Log.make ["protocols";"say"]

external core_exit : int -> 'a = "unix_exit"

let time = Unix.gettimeofday

let parse_arg s =
  try
    let i = String.index s '/' in
    let l = String.length s in
      (String.sub s 0 i),
      (String.sub s (i+1) (l-i-1))
  with
    | _ -> "",s

let say s ~log maxtime =
  let local = Filename.temp_file "say" ".wav" in
  (* Note that if liquidsoap gets killed while resolving this URI,
   * the empty tempfile remains. It is only cleaned if a temporary
   * indicator is successfully created from it and added to the request. *)
  let cmd = conf_program#get in
  let voice,s = parse_arg s in
    try
      let _,ret =
        dlog#f 3 "Synthetizing %S to %S" s local ;
        let pid = flush_all () ; fork () in
          if pid = 0 then begin
            try
              Sys.set_signal
                Sys.sigalrm (Sys.Signal_handle (fun _ -> core_exit 2)) ;
              let a = Unix.alarm (int_of_float (maxtime -. time ())) in
              assert (a = 0) ;
              if voice <> "" then
                execv cmd [| cmd;s;local;voice |]
              else
                execv cmd [| cmd;s;local |]
            with
              | _ -> core_exit 1
          end else
            waitpid [] pid
      in
        if ret = Unix.WEXITED 0 then
          [Request.indicator ~temporary:true local]
        else
          failwith "synthesis script returned an error"
    with
      | e ->
          (* This could for example be ENOMEM raised by Unix.fork. *)
          dlog#f 3
            "Failed to synthetize speech: %s!"
            (match e with Failure s -> s | _ -> Printexc.to_string e) ;
          log "Speech synthesis failed!" ;
          (try Unix.unlink local with _ -> ()) ;
          []

let () =
  Request.protocols#register
    ~sdoc:"Speech synthesis, with optional voice choice using say:voice/blah."
    "say"
    { Request.resolve = say ; Request.static = true }

let time arg ~log:_ _ =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let date =
    Printf.sprintf
      "%d:%d"
      tm.Unix.tm_hour
      tm.Unix.tm_min
  in
  let sentence = Str.global_replace (Str.regexp "\\$(time)") date arg in
    [Request.indicator ("say:"^sentence)]

let () =
  Request.protocols#register
    "time"
    ~sdoc:("Speech synthesis of a message where $(time) is replaced by "^
           "the current time")
    { Request.resolve = time ; Request.static = false }
