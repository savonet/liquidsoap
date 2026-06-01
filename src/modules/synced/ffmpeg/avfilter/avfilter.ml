(** This module provides an API to AVfilter. *)

type ground_arg =
  [ `String of string
  | `Int of int
  | `Int64 of int64
  | `Float of float
  | `Rational of Avutil.rational ]

type valued_arg = [ ground_arg | `Array of ground_arg list ]
type args = [ `Flag of string | `Pair of string * valued_arg ]

external get_array_separator : filter_name:string -> option_name:string -> char
  = "ocaml_avfilter_get_array_separator"

type ('a, 'b) av = { audio : 'a; video : 'b }
type ('a, 'b) io = { inputs : 'a; outputs : 'b }
type _config
type filter_ctx

type ('a, 'b, 'c) pad = {
  pad_name : string;
  filter_name : string;
  media_type : 'b;
  idx : int;
  filter_ctx : filter_ctx option;
  _config : _config option;
}

type ('a, 'b) pads =
  (('a, [ `Audio ], 'b) pad list, ('a, [ `Video ], 'b) pad list) av

type flag =
  [ `Dynamic_inputs
  | `Dynamic_outputs
  | `Slice_threads
  | `Support_timeline_generic
  | `Support_timeline_internal ]

type 'a filter = {
  name : string;
  description : string;
  options : Avutil.Options.t;
  flags : flag list;
  io : (('a, [ `Input ]) pads, ('a, [ `Output ]) pads) io;
}

type 'a input = [ `Frame of 'a Avutil.frame | `Flush ] -> unit
type 'a context = filter_ctx
type 'a output = { context : 'a context; handler : unit -> 'a Avutil.frame }
type 'a entries = (string * 'a) list
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) av
type outputs = ([ `Audio ] output entries, [ `Video ] output entries) av
type t = (inputs, outputs) io

type config = {
  c : _config;
  mutable names : string list;
  mutable video_inputs : filter_ctx entries;
  mutable audio_inputs : filter_ctx entries;
  mutable video_outputs : filter_ctx entries;
  mutable audio_outputs : filter_ctx entries;
}

external time_base : filter_ctx -> Avutil.rational
  = "ocaml_avfilter_buffersink_get_time_base"

external frame_rate : filter_ctx -> Avutil.rational
  = "ocaml_avfilter_buffersink_get_frame_rate"

external width : filter_ctx -> int = "ocaml_avfilter_buffersink_get_w"
external height : filter_ctx -> int = "ocaml_avfilter_buffersink_get_h"

external pixel_aspect : filter_ctx -> Avutil.rational option
  = "ocaml_avfilter_buffersink_get_pixel_aspect"

external pixel_format : filter_ctx -> Avutil.Pixel_format.t
  = "ocaml_avfilter_buffersink_get_pixel_format"

external channels : filter_ctx -> int = "ocaml_avfilter_buffersink_get_channels"

external channel_layout : filter_ctx -> Avutil.Channel_layout.t
  = "ocaml_avfilter_buffersink_get_channel_layout"

external sample_rate : filter_ctx -> int
  = "ocaml_avfilter_buffersink_get_sample_rate"

external sample_format : filter_ctx -> Avutil.Sample_format.t
  = "ocaml_avfilter_buffersink_get_sample_format"

external set_frame_size : filter_ctx -> int -> unit
  = "ocaml_avfilter_buffersink_set_frame_size"

exception Exists

type ('a, 'b, 'c) _filter = {
  _name : string;
  _description : string;
  _inputs : ('a, 'b, 'c) pad array;
  _outputs : ('a, 'b, 'c) pad array;
  _options : Avutil.Options.t;
  _flags : int;
}

external register_all : unit -> unit = "ocaml_avfilter_register_all"

let () = register_all ()

external get_all_filters : unit -> ([ `Unattached ], 'a, 'c) _filter array
  = "ocaml_avfilter_get_all_filters"

external int_of_flag : flag -> int = "ocaml_avfilter_int_of_flag"

let split_pads pads =
  let audio, video =
    Array.fold_left
      (fun (a, v) pad ->
        if pad.media_type = `Audio then (
          let pad : (_, [ `Audio ], _) pad = { pad with media_type = `Audio } in
          (pad :: a, v))
        else (
          let pad : (_, [ `Video ], _) pad = { pad with media_type = `Video } in
          (a, pad :: v)))
      ([], []) pads
  in

  let audio = List.sort (fun pad1 pad2 -> compare pad1.idx pad2.idx) audio in
  let video = List.sort (fun pad1 pad2 -> compare pad1.idx pad2.idx) video in

  { audio; video }

let filters, abuffer, buffer, abuffersink, buffersink =
  let filters, abuffer, buffer, abuffersink, buffersink =
    Array.fold_left
      (fun (filters, abuffer, buffer, abuffersink, buffersink)
           { _name; _description; _options; _inputs; _outputs; _flags } ->
        let io =
          { inputs = split_pads _inputs; outputs = split_pads _outputs }
        in
        let flags =
          List.filter
            (fun flag -> int_of_flag flag land _flags <> 0)
            [
              `Dynamic_inputs;
              `Dynamic_outputs;
              `Slice_threads;
              `Support_timeline_generic;
              `Support_timeline_internal;
            ]
        in
        let filter =
          {
            name = _name;
            description = _description;
            options = _options;
            io;
            flags;
          }
        in
        match _name with
          | s when s = "abuffer" ->
              (filters, Some filter, buffer, abuffersink, buffersink)
          | s when s = "buffer" ->
              (filters, abuffer, Some filter, abuffersink, buffersink)
          | s when s = "abuffersink" ->
              (filters, abuffer, buffer, Some filter, buffersink)
          | s when s = "buffersink" ->
              (filters, abuffer, buffer, abuffersink, Some filter)
          | _ -> (filter :: filters, abuffer, buffer, abuffersink, buffersink))
      ([], None, None, None, None)
      (get_all_filters ())
  in

  let sort = List.sort (fun f1 f2 -> compare f1.name f2.name) in

  let get_some = function
    | Some f -> f
    | None -> failwith "ffmpeg API error: missing buffer or sink!"
  in

  ( sort filters,
    get_some abuffer,
    get_some buffer,
    get_some abuffersink,
    get_some buffersink )

let find name = List.find (fun f -> f.name = name) filters
let find_opt name = List.find_opt (fun f -> f.name = name) filters
let pad_name { pad_name; _ } = pad_name
let filter_name { filter_name; _ } = filter_name

external init : unit -> _config = "ocaml_avfilter_init"

let init () =
  {
    c = init ();
    names = [];
    audio_inputs = [];
    video_inputs = [];
    audio_outputs = [];
    video_outputs = [];
  }

external create_filter :
  ?args:string ->
  name:string ->
  string ->
  _config ->
  filter_ctx * ('a, 'b, 'c) pad array * ('a, 'b, 'c) pad array
  = "ocaml_avfilter_create_filter"

let string_of_ground_arg = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Int64 i -> Int64.to_string i
  | `Float f -> string_of_float f
  | `Rational { Avutil.num; den } -> Printf.sprintf "%i/%i" num den

let rec args_of_args filter_name cur = function
  | [] -> cur
  | `Flag s :: args -> args_of_args filter_name (s :: cur) args
  | `Pair (lbl, (#ground_arg as g)) :: args ->
      args_of_args filter_name
        (Printf.sprintf "%s=%s" lbl (string_of_ground_arg g) :: cur)
        args
  | `Pair (lbl, `Array values) :: args ->
      let sep = get_array_separator ~filter_name ~option_name:lbl in
      let value_str =
        String.concat (String.make 1 sep) (List.map string_of_ground_arg values)
      in
      args_of_args filter_name
        (Printf.sprintf "%s=%s" lbl value_str :: cur)
        args

let args_of_args filter_name = function
  | Some args -> Some (String.concat ":" (args_of_args filter_name [] args))
  | None -> None

let attach_pad filter_ctx graph pad =
  { pad with filter_ctx = Some filter_ctx; _config = Some graph.c }

let append_io graph ~name filter_name filter_ctx =
  match filter_name with
    | "abuffer" ->
        graph.audio_inputs <- (name, filter_ctx) :: graph.audio_inputs
    | "buffer" -> graph.video_inputs <- (name, filter_ctx) :: graph.video_inputs
    | "abuffersink" ->
        graph.audio_outputs <- (name, filter_ctx) :: graph.audio_outputs
    | "buffersink" ->
        graph.video_outputs <- (name, filter_ctx) :: graph.video_outputs
    | _ -> ()

(* This creates a record with a hidden field in the last position. *)
external append_context :
  [ `Unattached ] filter -> filter_ctx -> [ `Attached ] filter
  = "ocaml_avfilter_append_context"

let attach ?args ~name filter graph =
  if List.mem name graph.names then raise Exists;
  let args = args_of_args filter.name args in
  let filter_ctx, inputs, outputs =
    create_filter ?args ~name filter.name graph.c
  in
  let io = { inputs = split_pads inputs; outputs = split_pads outputs } in
  let f () = List.map (attach_pad filter_ctx graph) in
  let inputs =
    { audio = (f ()) io.inputs.audio; video = (f ()) io.inputs.video }
  in
  let outputs =
    { audio = (f ()) io.outputs.audio; video = (f ()) io.outputs.video }
  in
  let io = { inputs; outputs } in
  let filter = { filter with io } in
  graph.names <- name :: graph.names;
  append_io graph ~name filter.name filter_ctx;
  append_context filter filter_ctx

external link : filter_ctx -> int -> filter_ctx -> int -> unit
  = "ocaml_avfilter_link"

let get_some = function Some x -> x | None -> assert false

let link src dst =
  link (get_some src.filter_ctx) src.idx (get_some dst.filter_ctx) dst.idx

type command_flag = [ `Fast ]

(* For now.. *)
let int_of_flag = function `Fast -> 1

external process_command :
  flags:int -> cmd:string -> arg:string -> filter_ctx -> string
  = "ocaml_avfilter_process_commands"

external get_context : [ `Attached ] filter -> filter_ctx
  = "ocaml_avfilter_get_content"

let process_command ?(flags = []) ~cmd ?(arg = "") filter =
  let flags =
    List.fold_left (fun cur flag -> cur land int_of_flag flag) 0 flags
  in
  process_command ~flags ~cmd ~arg (get_context filter)

type ('a, 'b, 'c) parse_node = {
  node_name : string;
  node_args : args list option;
  node_pad : ('a, 'b, 'c) pad;
}

type ('a, 'b) parse_av =
  ( ('a, [ `Audio ], 'b) parse_node list,
    ('a, [ `Video ], 'b) parse_node list )
  av

type 'a parse_io = (('a, [ `Input ]) parse_av, ('a, [ `Output ]) parse_av) io

external parse :
  inputs:(string * filter_ctx * int) array ->
  outputs:(string * filter_ctx * int) array ->
  string ->
  _config ->
  unit = "ocaml_avfilter_parse"

let parse ({ inputs; outputs } : [ `Attached ] parse_io) filters graph =
  let get_ctx (type a b) (node : ([ `Attached ], a, b) parse_node) =
    let { node_name; node_pad; _ } = node in
    let { filter_ctx; idx; _ } = node_pad in
    let ctx =
      match filter_ctx with
        | Some ctx -> ctx
        | None -> failwith "parse: unattached pad"
    in
    (node_name, ctx, idx)
  in
  let inputs =
    Array.of_list (List.map get_ctx inputs.audio @ List.map get_ctx inputs.video)
  in
  let outputs =
    Array.of_list
      (List.map get_ctx outputs.audio @ List.map get_ctx outputs.video)
  in
  parse ~inputs ~outputs filters graph.c

external config : _config -> unit = "ocaml_avfilter_config"

(* First argument is not used but here to make sure that _config is not GCed while
   using the filters. *)
external write_frame : _config -> filter_ctx -> 'a Avutil.frame -> unit
  = "ocaml_avfilter_write_frame"

external write_eof_frame : _config -> filter_ctx -> unit
  = "ocaml_avfilter_write_eof_frame"

let write_frame config filter = function
  | `Frame frame -> write_frame config filter frame
  | `Flush -> write_eof_frame config filter

(* First argument is not used but here to make sure that _config is not GCed while
   using the filters. *)
external get_frame : _config -> filter_ctx -> 'b Avutil.frame
  = "ocaml_avfilter_get_frame"

let launch graph =
  config graph.c;
  let audio =
    List.map
      (fun (name, filter_ctx) -> (name, write_frame graph.c filter_ctx))
      graph.audio_inputs
  in
  let video =
    List.map
      (fun (name, filter_ctx) -> (name, write_frame graph.c filter_ctx))
      graph.video_inputs
  in
  let inputs = { audio; video } in
  let audio =
    List.map
      (fun (name, filter_ctx) ->
        ( name,
          {
            context = filter_ctx;
            handler = (fun () -> get_frame graph.c filter_ctx);
          } ))
      graph.audio_outputs
  in
  let video =
    List.map
      (fun (name, filter_ctx) ->
        ( name,
          {
            context = filter_ctx;
            handler = (fun () -> get_frame graph.c filter_ctx);
          } ))
      graph.video_outputs
  in
  let outputs = { audio; video } in
  { outputs; inputs }

module Utils = struct
  type audio_converter = {
    time_base : Avutil.rational;
    filter_in : [ `Frame of Avutil.audio Avutil.frame | `Flush ] -> unit;
    filter_out : unit -> Avutil.audio Avutil.frame;
  }

  type audio_params = {
    sample_rate : int;
    channel_layout : Avutil.Channel_layout.t;
    sample_format : Avutil.Sample_format.t;
  }

  let init_audio_converter ?out_params ?out_frame_size ~in_time_base ~in_params
      () =
    let abuffer_args =
      [
        `Pair ("sample_rate", `Int in_params.sample_rate);
        `Pair ("time_base", `Rational in_time_base);
        `Pair
          ( "channel_layout",
            `String
              (Avutil.Channel_layout.get_description in_params.channel_layout)
          );
        `Pair
          ( "sample_fmt",
            `Int (Avutil.Sample_format.get_id in_params.sample_format) );
      ]
    in

    let graph = init () in
    let abuffer = attach ~args:abuffer_args ~name:"abuffer" abuffer graph in

    let output =
      match abuffer.io.outputs.audio with o :: _ -> o | _ -> assert false
    in

    let output =
      match out_params with
        | None -> output
        | Some out_params ->
            let aresample = find "aresample" in
            let args =
              [
                `Pair ("in_sample_rate", `Int in_params.sample_rate);
                `Pair
                  ( "in_chlayout",
                    `String
                      (Avutil.Channel_layout.get_description
                         in_params.channel_layout) );
                `Pair
                  ( "in_sample_fmt",
                    `Int (Avutil.Sample_format.get_id in_params.sample_format)
                  );
                `Pair ("out_sample_rate", `Int out_params.sample_rate);
                `Pair
                  ( "out_chlayout",
                    `String
                      (Avutil.Channel_layout.get_description
                         out_params.channel_layout) );
                `Pair
                  ( "out_sample_fmt",
                    `Int (Avutil.Sample_format.get_id out_params.sample_format)
                  );
              ]
            in
            let aresample = attach ~args ~name:"aresample" aresample graph in
            let ainput, aoutput =
              match (aresample.io.inputs.audio, aresample.io.outputs.audio) with
                | i :: _, o :: _ -> (i, o)
                | _ -> assert false
            in
            link output ainput;
            aoutput
    in

    let abuffersink = attach ~name:"sink" abuffersink graph in

    let () =
      match abuffersink.io.inputs.audio with
        | input :: _ -> link output input
        | _ -> assert false
    in
    let filter = launch graph in
    let filter_in, filter_out =
      match (filter.inputs.audio, filter.outputs.audio) with
        | (_, i) :: _, (_, o) :: _ -> (i, o)
        | _ -> assert false
    in
    let () =
      match out_frame_size with
        | None -> ()
        | Some frame_size -> set_frame_size filter_out.context frame_size
    in
    let time_base = time_base filter_out.context in
    { time_base; filter_in; filter_out = filter_out.handler }

  let convert_audio { filter_in; filter_out; _ } cb frame =
    let rec flush () =
      try
        cb (filter_out ());
        flush ()
      with Avutil.Error `Eagain -> ()
    in
    filter_in frame;
    try flush () with Avutil.Error `Eof when frame = `Flush -> ()

  let time_base { time_base; _ } = time_base
end
