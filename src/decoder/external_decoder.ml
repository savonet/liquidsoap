(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2014 Savonet team

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

(** Decode files using an external decoder. *)

let priority = Tutils.Blocking
let buf_size = 1024

(** First, an external decoder that receives
  * on its stdin. *)

(** This function is used to wrap around the "real" input.
  * It pipes its data to the external process and read
  * the available output. *)
let external_input process input =
  (** Open the external process, get its stdin/stdout *)
  let pull,push = Unix.open_process process in
  (** We operate on the Unix descriptors of 
    * the processe's stdio. *)
  let push_e = Unix.descr_of_out_channel push in
  let pull_e = Unix.descr_of_in_channel pull in
  (** We also need a pipe to wake up the task
    * when we want to force it to terminate. *)
  let pull_p,push_p = Unix.pipe () in
  (** We gonna wait on these events. *)
  let events = [`Read pull_p; `Write push_e] in
  (** These variables are used to synchronize 
    * between the main thread and the task's thread
    * when we terminate the task.. *)
  let is_task = ref true in
  let task_m = Mutex.create () in
  let task_c = Condition.create () in
  (** The function used to close the task. *)
  let close_task () =
    (** First, close the processes' stdin
      * as well as the task's side of the pipe. *)
    Unix.close pull_p ;
    Unix.close push_e ;
    (** Now grab the synchronization
      * lock, set is_task to false
      * and signal it to wake-up the main
      * thread waiting for the task to end.. *)
    Tutils.mutexify task_m (fun () ->
      is_task := false ;
      Condition.signal task_c) () ;
    (* We can now close the process. *)
    ignore(Unix.close_process (pull,push));
    (** Finally, tell duppy that we are done
      * by returning an empty list of new tasks. *)
    []
  in
  (** The main task function. (rem,ofs,len)
    * is the remaining string to write. *)
  let rec task (rem,ofs,len) l =
   let rem,ofs,len = 
      (* If we are done with the current string,
       * try to get a new one from the original input. *)
      if len = 0 then
        let s,read = input.Decoder.read buf_size in
        s,0,read
      else
        rem,ofs,len
   in
   let must_close = List.mem (`Read pull_p) l in
   (* If we could not get something to write or
    * if the close pipe contains something, we 
    * close the task. *)
   if len = 0 || must_close then begin
     close_task ()
   end else
     try
      (* Otherwise, we write and keep track of 
       * what was not written yet. *)
      let written = Unix.write push_e rem ofs len in
        if written <= 0 then close_task () else
          [{ Duppy.Task.
              priority = priority;
              events   = events;
              handler  = task (rem,ofs+written,len-written)
          }]
     with _ ->
       close_task ()
  in
    (** This initiates the task. *)
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority;
          events   = events;
          handler  = task ("",0,0)
      } ;
    (* Now the new input, which reads the process's output *)
    { Decoder.
        read = 
         (fun inlen ->
           Tutils.mutexify task_m (fun () ->
             if !is_task then
               let tmpbuf = String.create inlen in
               let read = Unix.read pull_e tmpbuf 0 inlen in
               tmpbuf, read
              else
                "", 0) ());
         tell = None;
         length = None;
         lseek = None },
    (* And a function to close the process *)
    (fun () -> 
      (* We grab the task's mutex. *)
      Tutils.mutexify task_m (fun () ->
        (* If the task has not yet ended, 
         * we write a char in the close pipe 
         * and wait for a signal from the task. *)
        if !is_task then
          begin
            ignore(Unix.write push_p " " 0 1) ;
            Condition.wait task_c task_m;
          end;
          (* Now we can close our side of 
           * the close pipe. *)
          Unix.close push_p) ())

let duration process = 
  let pull = Unix.open_process_in process in
  let w = Wav_aiff.in_chan_read_header pull in
  let ret = Wav_aiff.duration w in
  ignore(Unix.close_process_in pull) ;
  ret

module Generator = Generator.From_audio_video
module Buffered = Decoder.Buffered(Generator)

(** A function to wrap around the Wav_aiff_decoder *)
let create process kind filename = 
  let close = ref (fun () -> ()) in
  let create input =
    let input,actual_close = external_input process input in
      close := actual_close ;
      Wav_aiff_decoder.D.create ?header:None input
  in
  let generator = Generator.create `Audio in
  let dec = Buffered.file_decoder filename kind create generator in
  { dec with
     Decoder.close = 
      (fun () ->
         Tutils.finalize
          ~k:(fun () -> dec.Decoder.close ()) 
          !close) }

let create_stream process input =
  let input,close = external_input process input in
  (* Put this here so that ret is not in its closure.. *)
  let close _ = close () in
  let ret = Wav_aiff_decoder.D_stream.create input in
  Gc.finalise close ret;
  ret

let test_kind f filename = 
  (* 0 = file rejected,
   * n<0 = file accepted, unknown number of audio channels,
   * n>0 = file accepted, known number of channels. *)
  let ret = f filename in
    if ret = 0 then None else
      Some { Frame. video = Frame.Zero ; midi = Frame.Zero ;
                    audio =
                      if ret < 0 then
                        Frame.Succ Frame.Variable
                      else
                        Frame.mul_of_int ret }

let register_stdin name sdoc mimes test process =
  Decoder.file_decoders#register name ~sdoc
    (fun ~metadata:_ filename kind ->
       match test_kind test filename with
         | None -> None
         | Some out_kind ->
             (* Check that kind is more permissive than out_kind and
              * declare that our decoding function will respect out_kind. *)
             if Frame.kind_sub_kind out_kind kind then
               Some (fun () -> create process out_kind filename)
             else None) ;
  let duration filename = 
    let process = 
      Printf.sprintf "cat %s | %s" (Filename.quote filename) process
    in
    duration process
  in
  Request.dresolvers#register name duration;
  if mimes <> [] then
    Decoder.stream_decoders#register name
      ~sdoc:(Printf.sprintf 
        "Use %s to decode any stream with an appropriate MIME type."
        name)
       (fun mime kind ->
          let (<:) a b = Frame.mul_sub_mul a b in
            if List.mem mime mimes &&
               (* Check that it is okay to have zero video and midi,
                * and at least one audio channel. *)
               Frame.Zero <: kind.Frame.video &&
               Frame.Zero <: kind.Frame.midi &&
               kind.Frame.audio <> Frame.Zero
            then
              (* In fact we can't be sure that we'll satisfy the content
               * kind, because the stream might be mono or stereo.
               * For now, we let this problem result in an error at
               * decoding-time. Failing early would only be an advantage
               * if there was possibly another plugin for decoding
               * correctly the stream (e.g. by performing conversions). *)
              Some (create_stream process)
            else
              None)

(** Now an external decoder that directly operates
  * on the file. The remaining time in this case
  * can only be approximative. It is -1 while
  * the file is being decoded and the length
  * of the buffer when the external decoder
  * has exited. *)

let log = Dtools.Log.make ["decoder";"external";"oblivious"]

let external_input_oblivious process filename prebuf = 
  let process = process filename in
  let process_done = ref false in
  let pull = Unix.open_process_in process in
  let pull_e = Unix.descr_of_in_channel pull in
  let close () =
    if not !process_done then
     begin
      ignore(Unix.close_process_in pull);
      process_done := true
     end
  in
  let read len = 
    if not !process_done then
      let ret = String.create len in
      let read = Unix.read pull_e ret 0 len in
      if read = 0 then close () ; 
      ret,read
    else
      "",0
  in
  let input = 
    { Decoder.
        read = read;
        tell = None;
        length = None;
        lseek = None }
  in
  let gen = Generator.create `Audio in
  let prebuf = Frame.master_of_seconds prebuf in
  let decoder = Wav_aiff_decoder.D.create input in
  let fill frame = 
     if not !process_done then
       begin try
         while Generator.length gen < prebuf && (not !process_done) do
           decoder.Decoder.decode gen
         done
       with
         | e ->
             log#f 4 "Decoding %s ended: %s." process (Printexc.to_string e) ;
             close ()
       end ;
     Generator.fill gen frame ;
     (** We return -1 while the process is not yet
       * finished. *)
    if !process_done then Generator.length gen else -1
  in 
  { Decoder.
     fill = fill ;
     fseek = decoder.Decoder.seek;
     close = close }

let register_oblivious name sdoc test process prebuf =
  Decoder.file_decoders#register name ~sdoc
    (fun ~metadata:_ filename kind ->
       match test_kind test filename with
         | None -> None
         | Some out_kind ->
             (* Check that kind is more permissive than out_kind and
              * declare that our decoding function will respect out_kind. *)
             if Frame.kind_sub_kind out_kind kind then
               Some (fun () -> external_input_oblivious process filename prebuf)
             else None);
  let duration filename = 
    duration (process filename)
  in
  Request.dresolvers#register name duration
