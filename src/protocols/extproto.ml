(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

open Dtools

let dlog = Log.make ["protocols";"external"]

let which =
  let which = Utils.which ~path:Configure.path in
  if Sys.os_type <> "Win32" then
    which
  else
    fun s ->
      try which s with Not_found ->
        which (Printf.sprintf "%s%s" s Configure.exe_ext)

(* Find extension of a file based on a returned content-type. *)
let ext_of_content_type url =
  if Sys.os_type = "Win32" then
    raise Not_found;
  let curl = which "curl" in
  (* Using curl here since it's way more powerful. *)
  let cmd =
    Printf.sprintf
      "%s -I -X HEAD -L %s 2>/dev/null | grep -i '^content-type' | tail -n 1 | cut -d':' -f 2 | cut -d';' -f 1"
      curl url
  in
  let ch = Unix.open_process_in cmd in
  let mime = String.trim (input_line ch) in
  ignore(Unix.close_process_in ch);
  match Utils.StringCompat.lowercase_ascii mime with
    | "audio/mpeg" -> "mp3"
    | "application/ogg" | "application/x-ogg"
    | "audio/x-ogg" | "audio/ogg"
    | "video/ogg" -> "ogg"
    | "audio/x-flac" -> "flac"
    | "audio/mp4" | "application/mp4" -> "mp4"
    | "audio/vnd.wave" | "audio/wav"
    | "audio/wave" | "audio/x-wav" -> "wav"
    | _ -> raise Not_found

let get_ext src =
  try
    ext_of_content_type src
  with Not_found -> Utils.get_ext src

let mktmp src =
  let file_ext =
    Printf.sprintf ".%s"
    (try
      get_ext src
     with
       | _ -> "osb")
  in
  Filename.temp_file "liq" file_ext

let resolve proto program command s ~log maxtime =
  let s = proto ^ ":" ^ s in
  (* We create a fresh stdin for the process,
   * and another one, unused by the child, on which we'll wait for EOF
   * as a mean to detect termination. *)
  let (iR,iW) = Unix.pipe () in
  let (xR,xW) = Unix.pipe () in
  let local = mktmp s in
  let args,active =
    match command program s local with
      | `Active args  -> args,true
      | `Passive args -> args,false 
  in
  let pid =
    Unix.create_process program args iR xW Unix.stderr
  in
  dlog#f 4 "Executing %s %S %S" program s local;
  let timeout () = max 0. (maxtime -. Unix.gettimeofday ()) in
  Unix.close iR ;
  let prog_stdout = ref "" in
  (* Setup task.. *)
  let after_task,task_done =
    let m = Mutex.create () in
    let c = Condition.create () in
    let is_task = ref false in
    Tutils.mutexify m (fun fn ->
      if !is_task then
        Condition.wait c m;
      fn()),
    Tutils.mutexify m (fun () ->
      is_task := false;
      Condition.signal c)
  in
  let rec task () = 
    let timeout = timeout () in
    { Duppy.Task.
      priority = Tutils.Non_blocking;
      events   = [`Read xR; `Delay timeout];
      handler  = fun l ->
        let rem = 
          if List.mem (`Delay timeout) l then
           begin
            Unix.kill pid 9;
            []
           end
          else
           begin
            let s = Bytes.create 1024 in
            let ret =
              try Unix.read xR s 0 1024 with _ -> 0
            in
            prog_stdout := !prog_stdout ^ (Bytes.sub s 0 ret);
            if ret > 0 then [task()] else []
           end
        in
        if rem = [] then task_done();
        rem }
  in
  Duppy.Task.add Tutils.scheduler (task ());
  let (p,code) = Unix.waitpid [] pid in
  assert (p <> 0) ;
  dlog#f 4 "Download process finished (%s)"
    (match code with
       | Unix.WSIGNALED _ -> "killed"
       | Unix.WEXITED 0 -> "ok"
       | _ -> "error") ;
  Unix.close iW ;
  Unix.close xW ;
  after_task (fun () ->
    let local =
      if active then !prog_stdout else local
    in
    if code = Unix.WEXITED 0 then
    [Request.indicator ~temporary:true local]
    else begin
      log "Download failed: timeout, invalid URI ?" ;
      ( try Unix.unlink !prog_stdout with _ -> () ) ;
      []
    end)

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "extproto") "External protocol resolvers"
    ~comments:["Settings for the external protocol resolver"]

let conf_server_name =
  Dtools.Conf.bool ~p:(conf#plug "use_server_name") "Use server-provided name"
    ~d:false ~comments:["Use server-provided name."]

let get_program, command =
  try
    which Configure.get_program,
    (fun prog src dst ->
      if conf_server_name#get then
        (`Active [|prog;src;dst;"true"|])
      else
        (`Passive [|prog;src;dst|]))
  with
    | Not_found ->
        "curl", (fun prog src dst ->
          (`Passive [|prog;"-sL";src;"-o";dst|]))

let extproto = [
  get_program,
  [ "http";"https";"ftp" ],
  command
]

let () =
  (* Enabling of protocols rely on the presence of the programs.
   * The detection must be done at startup, so that --list-plugins shows the
   * enabled protocols. But we delay logging for Init.at_start time, so that
   * logs shows enabled/disabled protocols. *)
  List.iter
    (fun (prog,protos,command) ->
       try
         let prog = which prog in
           dlog#f 3 "Found %S." prog ;
           List.iter
             (fun proto ->
                Request.protocols#register
                  ~sdoc:(Printf.sprintf "Fetch files using %S." prog)
                  proto
                  { Request.resolve = resolve proto prog command ;
                    Request.static = false })
             protos
       with
         | Not_found ->
             dlog#f 3 "Didn't find %S." prog
    )
    extproto
