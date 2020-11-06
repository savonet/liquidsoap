(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

class input ~bufferize ~log_overfull ~kind ~start ~on_start ~on_stop ?format
  ~opts url =
  let max_ticks = 2 * Frame.master_of_seconds bufferize in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  object (self)
    inherit Source.source ~name:"input.ffmpeg" kind

    inherit
      Generated.source
        (Generator.create ~log ~log_overfull ~overfull:(`Drop_old max_ticks)
           `Undefined)
        ~empty_on_abort:false ~bufferize

    inherit
      Start_stop.async ~name:"input.ffmpeg" ~on_start ~on_stop ~autostart:start

    val mutable container = None

    method private close_container =
      match container with
        | None -> ()
        | Some input ->
            Av.close input;
            container <- None

    method private get_decoder =
      self#close_container;
      let opts = Hashtbl.copy opts in
      let input = Av.open_input ?format ~opts url in
      if Hashtbl.length opts > 0 then
        failwith
          (Printf.sprintf "Unrecognized options: %s"
             (Ffmpeg_format.string_of_options opts));
      let content_type = Ffmpeg_decoder.get_type ~ctype:self#ctype ~url input in
      if not (Decoder.can_decode_type content_type self#ctype) then
        failwith
          (Printf.sprintf "url %S cannot produce content of type %s" url
             (Frame.string_of_content_type self#ctype));
      container <- Some input;
      let audio, video = Ffmpeg_decoder.mk_streams ~ctype:self#ctype input in
      Ffmpeg_decoder.mk_decoder ?audio ?video ~target_position:(ref None) input

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
      let kill, wait = Tutils.stoppable_thread self#feed "Ffmpeg input" in
      kill_feeding <- Some kill;
      wait_feeding <- Some wait

    method private stop =
      (Option.get kill_feeding) ();
      kill_feeding <- None

    method private output_reset =
      self#stop;
      self#start

    method private is_active = true

    method private stype = Source.Fallible

    method private feed (should_stop, has_stopped) =
      try
        let decoder = self#get_decoder in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while true do
          if should_stop () then failwith "stop";
          decoder buffer
        done
      with e ->
        Generator.add_break ~sync:true generator;
        self#close_container;
        begin
          match e with
          | Failure s -> self#log#severe "Feeding stopped: %s." s
          | e -> self#log#severe "Feeding stopped: %s." (Printexc.to_string e)
        end;
        if should_stop () then has_stopped ()
        else self#feed (should_stop, has_stopped)
  end

let parse_args ~t name p opts =
  let name = name ^ "_args" in
  let args = List.assoc name p in
  let args = Lang.to_list args in
  let extract_pair extractor v =
    let label, value = Lang.to_product v in
    Hashtbl.add opts (Lang.to_string label) (extractor value)
  in
  let extract =
    match t with
      | `Int -> fun v -> extract_pair (fun v -> `Int (Lang.to_int v)) v
      | `Float -> fun v -> extract_pair (fun v -> `Float (Lang.to_float v)) v
      | `String -> fun v -> extract_pair (fun v -> `String (Lang.to_string v)) v
  in
  List.iter extract args

let () =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  let args ?t name =
    let t =
      match t with
        | Some t -> Lang.product_t Lang.string_t t
        | None -> Lang.string_t
    in
    (name ^ "_args", Lang.list_t t, Some (Lang.list []), None)
  in
  Lang.add_operator "input.ffmpeg" ~active:true
    ~descr:"Decode a url using ffmpeg." ~category:Lang.Input
    ( Start_stop.input_proto
    @ [
        args ~t:Lang.int_t "int";
        args ~t:Lang.float_t "float";
        args ~t:Lang.string_t "string";
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 5.),
          Some "Duration of buffered data before starting playout." );
        ( "log_overfull",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Log when the source's buffer is overfull." );
        ( "format",
          Lang.string_t,
          Some (Lang.string ""),
          Some
            "Force a specific input format. Autodetected when passed an empty \
             string." );
        ("", Lang.string_t, None, Some "URL to decode.");
      ] )
    ~return_t:k
    (fun p ->
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let format = Lang.to_string (List.assoc "format" p) in
      let format =
        if format = "" then None
        else (
          match Av.Format.find_input_format format with
            | Some f -> Some f
            | None ->
                raise
                  (Lang_errors.Invalid_value
                     ( Lang.string format,
                       "Could not find ffmpeg input format with that name" )) )
      in
      let opts = Hashtbl.create 10 in
      parse_args ~t:`Int "int" p opts;
      parse_args ~t:`Float "float" p opts;
      parse_args ~t:`String "string" p opts;
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let url = Lang.to_string (Lang.assoc "" 1 p) in
      ( new input
          ~kind ~start ~on_start ~on_stop ~bufferize ~log_overfull ?format ~opts
          url
        :> Source.source ))
