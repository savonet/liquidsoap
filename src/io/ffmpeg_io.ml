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

module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

class input ~bufferize ~kind ~start ~on_start ~on_stop url =
  let max_ticks = 2 * Frame.master_of_seconds bufferize in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  object (self)
    inherit Source.source ~name:"input.ffmpeg" kind

    inherit
      Generated.source
        (Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined)
        ~empty_on_abort:false ~bufferize

    inherit
      Start_stop.async
        ~source_kind:"ffmpeg" ~name:"input.ffmpeg" ~on_start ~on_stop
          ~autostart:start

    val mutable container = None

    method private close_container =
      match container with
        | None -> ()
        | Some input ->
            Av.close input;
            container <- None

    method private get_decoder =
      self#close_container;
      let input = Av.open_input url in
      let content_type = Ffmpeg_decoder.get_type ~url input in
      if not (Frame.type_has_kind content_type kind) then
        failwith
          (Printf.sprintf "url %S cannot product content of type %s" url
             (Frame.string_of_content_type content_type));
      container <- Some input;
      let audio =
        try
          Some
            (Ffmpeg_decoder.mk_audio_decoder ~put_audio:Generator.put_audio
               input)
        with Avutil.Error _ -> None
      in
      let video =
        try
          Some
            (Ffmpeg_decoder.mk_video_decoder ~put_video:Generator.put_video
               input)
        with Avutil.Error _ -> None
      in
      Ffmpeg_decoder.mk_decoder ~set_mode:Generator.set_mode
        ~add_break:Generator.add_break ~audio ~video ~container:input

    val mutable kill_feeding = None

    val mutable wait_feeding = None

    method private start =
      begin
        match wait_feeding with
        | None -> ()
        | Some f ->
            f ();
            wait_feeding <- None
      end;
      let kill, wait = Tutils.stoppable_thread self#feed "UDP input" in
      kill_feeding <- Some kill;
      wait_feeding <- Some wait

    method private stop =
      (Utils.get_some kill_feeding) ();
      kill_feeding <- None

    method private output_reset =
      self#stop;
      self#start

    method private is_active = true

    method private stype = Source.Fallible

    method private feed (should_stop, has_stopped) =
      try
        let decoder = self#get_decoder in
        while true do
          if should_stop () then failwith "stop";
          decoder generator
        done
      with e ->
        Generator.add_break ~sync:`Drop generator;
        self#close_container;
        begin
          match e with
          | Failure s -> self#log#severe "Feeding stopped: %s." s
          | e -> self#log#severe "Feeding stopped: %s." (Printexc.to_string e)
        end;
        if should_stop () then has_stopped ()
        else self#feed (should_stop, has_stopped)
  end

let () =
  let k = Lang.univ_t () in
  Lang.add_operator "input.ffmpeg" ~active:true
    ~descr:"Decode a url using ffmpeg." ~category:Lang.Input
    ( Start_stop.input_proto
    @ [
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 1.),
          Some "Duration of buffered data before starting playout." );
        ("", Lang.string_t, None, Some "URL to decode.");
      ] )
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let url = Lang.to_string (Lang.assoc "" 1 p) in
      (new input ~kind ~start ~on_start ~on_stop ~bufferize url :> Source.source))
