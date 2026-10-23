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

exception Source_fault of string

type output = {
  field : Frame.field;
  buffer : Decoder.buffer;
  place : int -> unit;
}

type transform = {
  base : int;
  push : input_time:int -> Content.data -> unit;
  drain : unit -> unit;
  discard : unit -> unit;
  close : unit -> unit;
}

type builder = output -> input:Source.source -> transform
type entry = [ `Metadata of Frame.metadata | `Track_mark ]

(* The transform and what lives with it: the input time reached, the
   entries waiting for their output item, the last track mark relayed. *)
type running = {
  transform : transform;
  mutable input_time : int;
  pending : (int * entry) Queue.t;
  mutable last_mark : int option;
  mutable received : bool;
}

class producer ~name ~field ~check ~builder ~child_frame_type input_val =
  let input = Lang.to_source input_val in
  object (self)
    inherit
      Child_support.producer
        ~child_frame_type ~check_self_sync:false ~name input_val as super

    val mutable running = None
    val mutable faulted_until = None

    initializer
      self#child#set_process_frame (fun _ -> function
        | `Frame frame -> self#process frame
        | `Flush -> self#discard);
      Option.iter (fun check -> self#on_wake_up check) check

    method private write_entry =
      function
      | `Metadata m -> Generator.add_metadata self#child_buffer m
      | `Track_mark -> Generator.add_track_mark self#child_buffer

    method private place r t =
      let rec go () =
        match Queue.peek_opt r.pending with
          | Some (time, entry) when time <= t ->
              ignore (Queue.pop r.pending);
              self#write_entry entry;
              go ()
          | _ -> ()
      in
      go ()

    method private flush_entries r =
      Queue.iter (fun (_, entry) -> self#write_entry entry) r.pending;
      Queue.clear r.pending

    method private build =
      let output =
        {
          field;
          buffer = Decoder.mk_buffer ~ctype:self#content_type self#child_buffer;
          place = (fun t -> Option.iter (fun r -> self#place r t) running);
        }
      in
      let transform = builder output ~input in
      let r =
        {
          transform;
          input_time = transform.base;
          pending = Queue.create ();
          last_mark = None;
          received = false;
        }
      in
      running <- Some r;
      r

    method private end_running ~drain =
      match running with
        | None -> ()
        | Some r ->
            running <- None;
            (try if drain then r.transform.drain () else r.transform.discard ()
             with exn -> (
               self#log#important "Draining failed: %s" (Printexc.to_string exn);
               try r.transform.discard () with _ -> ()));
            self#flush_entries r

    method private discard = self#end_running ~drain:false

    method private source_fault message =
      self#log#severe "Source fault on %s: %s" input#id message;
      self#end_running ~drain:true;
      Generator.add_track_mark self#child_buffer;
      if not self#fallible then
        Runtime_error.raise ~pos:(Option.to_list self#pos)
          ~message:
            (Printf.sprintf "%s cannot convert %s: %s" self#id input#id message)
          "ffmpeg"
      else
        faulted_until <- Some (Unix.gettimeofday () +. Faults.fault_retry_delay)

    method private faulted =
      match faulted_until with
        | Some t when Unix.gettimeofday () < t -> true
        | Some _ ->
            faulted_until <- None;
            false
        | None -> false

    method private relay r frame =
      List.iter
        (fun (position, m) ->
          Queue.push (r.input_time + position, `Metadata m) r.pending)
        (Frame.get_all_metadata frame);
      List.iter
        (fun position ->
          let time = r.input_time + position in
          if r.last_mark <> Some time then (
            r.last_mark <- Some time;
            Queue.push (time, `Track_mark) r.pending))
        (Frame.track_marks frame)

    method private process frame =
      if not self#faulted then (
        try
          let r = match running with Some r -> r | None -> self#build in
          self#relay r frame;
          let length = Frame.position frame in
          if length > 0 then (
            r.received <- true;
            r.transform.push ~input_time:r.input_time (Frame.get frame field));
          r.input_time <- r.input_time + length;
          if length = 0 then self#place r r.input_time
        with
          | Source_fault message -> self#source_fault message
          | Failure message -> self#source_fault message
          | Avutil.Error e -> self#source_fault (Avutil.string_of_error e))

    method! private can_generate_frame =
      Generator.length self#child_buffer > 0
      || (not self#faulted)
         && (self#child#is_ready
            || match running with Some r -> r.received | None -> false)

    (* The drain comes after the input stops filling the frame: a frame cut
       short before the drained output would end the track there. *)
    method! private generate_frame =
      let size = Lazy.Mutexed.force Frame.size in
      while self#child#is_ready && Generator.length self#child_buffer < size do
        self#child_tick
      done;
      if not self#child#is_ready then (
        if running <> None then self#end_running ~drain:true;
        self#child_tick);
      Generator.slice self#child_buffer size

    method! seek len =
      let buffered = min (Generator.length self#child_buffer) len in
      let moved = super#seek len in
      if moved > buffered then (
        Option.iter (fun r -> r.transform.close ()) running;
        self#discard);
      moved

    initializer self#on_sleep (fun () -> self#discard)
  end

let create ~name ~field ~input_t ~check ~builder input_val =
  let child_frame_type =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.add field input_t Frame.Fields.empty)
  in
  (new producer ~name ~field ~check ~builder ~child_frame_type input_val
    :> Source.source)
