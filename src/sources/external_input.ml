(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

module Generator = Float_pcm.Generator_from_raw
module Generated = Generated.From_Raw_pcm_Generator

(* {1 External Input handling} *)

exception Finished of string*bool

class external_input ~restart ~bufferize ~channels 
                     ~restart_on_error ~max 
                     ~samplerate command =
  let abg_max_len = Fmt.samples_of_seconds max in
  let samplesize = 16 in
  let big_endian = false in
  let signed = true in
  let in_freq = float samplerate in
  let out_freq = float (Fmt.samples_per_second()) in
  let channels = channels in
  let abg =
    Generator.create
      ~channels ~samplesize ~signed ~big_endian ~in_freq ~out_freq
  in
  let priority = Tutils.Non_blocking in
object (self)
  inherit Source.source
  inherit Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable should_stop = false

  method stype = Source.Fallible

  method wake_up _ =
    self#log#f 2 "Starting process.";
    let create () = 
      let in_e = Unix.open_process_in command in
      in_e,Unix.descr_of_in_channel in_e
    in
    let (in_e,in_d) as x = create () in
    let tmpbuf = String.create 1024 in
    let rec process ((in_e,in_d) as x) l = 
      let get_data () =
        let ret = input in_e tmpbuf 0 1024 in
        if ret = 0 then raise (Finished ("Process exited.",restart));
        Generator.feed abg (String.sub tmpbuf 0 ret)
      in
      let do_restart s restart f = 
        self#log#f 2 "%s" s;
        begin
         try
           ignore(Unix.close_process_in in_e);
         with
           | _ -> ()
        end;
        if restart then
         begin
          self#log#f 2 "Restarting process.";
          let ((in_e,in_d) as x) = create () in
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
        if should_stop then
          raise (Finished ("Source stoped: closing process.",false));
        let len = Generator.length abg - abg_max_len in
        if len >= 0 then
           let delay = Fmt.seconds_of_samples len in
           [`Delay delay]
        else
         begin
          if List.mem (`Read in_d) l then
           get_data ();
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
            (Printf.sprintf "Process exited with error: %s" 
                (Printexc.to_string e)) restart_on_error
                (fun () -> raise e)
    in
    let task = 
     { Duppy.Task.
        priority = priority;
        events   = [ `Read in_d];
        handler  = process x
     }
    in
    Duppy.Task.add Tutils.scheduler task 

  method sleep = should_stop <- true
end

let () =
    Lang.add_operator "input.external"
      ~category:Lang.Input
      ~descr:("Stream data from an external application.")
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
      (fun p _ ->
         let command = Lang.to_string (List.assoc "" p) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let channels = Lang.to_int (List.assoc "channels" p) in
         let samplerate = Lang.to_int (List.assoc "samplerate" p) in
         let restart = Lang.to_bool (List.assoc "restart" p) in
         let restart_on_error = 
           Lang.to_bool (List.assoc "restart_on_error" p) 
         in
         let max = Lang.to_float (List.assoc "max" p) in
          ((new external_input ~restart ~bufferize ~channels
                               ~restart_on_error ~max 
                               ~samplerate command):>Source.source))
