
(** Protocol plugin for speech synthesis *)

open Dtools
open Unix

let log = Log.log ~label:"say"

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

let say s req maxtime =
  let local = Filename.temp_file "say" ".wav" in
  let cmd = Configure.tts_program in
  let voice,s = parse_arg s in
  let pid,ret =
    log 3 (Log.f "Synthetizing %S to %S" s local) ;
    let pid = flush_all () ; fork () in
      if pid = 0 then (
	try
	  Sys.set_signal
	    Sys.sigalrm (Sys.Signal_handle (fun _ -> core_exit 2)) ;
	  assert (0 = Unix.alarm (int_of_float (maxtime -. time ()))) ;
	  if voice <> "" then
	    execv cmd [| cmd;s;local;voice |]
	  else
	    execv cmd [| cmd;s;local |] ;
	  core_exit 1
	with
	  | _ -> core_exit 1
      ) else
	waitpid [] pid
  in
    if ret = Unix.WEXITED 0 then (
      Request.push_indicator req [local] ;
      Request.set_metadata req "temporary" "true"
    ) else (
      Request.log req "Speech synthesis failed !" ;
      ( try Unix.unlink local with _ -> () )
    )

let _ =
  Request.protocols#register
    ~sdoc:("Speech synthesis. You can specify the voice by its name or id, "^
	   "using the syntax 'say:helen/hello, you ...'.")
    "say"
    { Request.resolve = say ; Request.static = true }

let at_re = Str.regexp "@"

let time arg req timeout =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let date =
    Printf.sprintf
      "%d:%d"
      tm.Unix.tm_hour
      tm.Unix.tm_min
  in
  let sentence = Str.global_replace at_re date arg in
    Request.push_indicator req ["say://"^sentence]

let _ =
  Request.protocols#register
    "time"
    ~sdoc:("'time:Now is @.' results in a speech synthesis, "^
           "where @ is replaced by the current time")
    { Request.resolve = time ; Request.static = false }
