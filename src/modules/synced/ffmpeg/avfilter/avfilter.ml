open Avutil

type config

type ground_arg =
  [ `String of string
  | `Int of int
  | `Int64 of int64
  | `Float of float
  | `Rational of rational ]

type valued_arg = [ ground_arg | `Array of ground_arg list ]
type args = [ `Flag of string | `Pair of string * valued_arg ]
type ('a, 'b) av = { audio : 'a; video : 'b }
type ('a, 'b) io = { inputs : 'a; outputs : 'b }

(* A filter instance inside its graph. Holding the graph keeps the instance
   valid; the C side checks, under the graph's guard, that the graph is not
   failed before it follows the pointer. *)
type instance
type attachment = { graph : config; instance : instance }

(* [index] is the position among all the pads of the filter in the pad's
   direction. An unattached pad has no attachment. *)
type ('a, 'b, 'c) pad = {
  pad_name : string;
  pad_filter : string;
  index : int;
  attachment : attachment option;
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

type 'a input = [ `Frame of 'a frame | `Flush ] -> unit
type 'a context = attachment
type 'a output = { context : 'a context; handler : unit -> 'a frame }
type 'a entries = (string * 'a) list
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) av
type outputs = ([ `Audio ] output entries, [ `Video ] output entries) av
type t = (inputs, outputs) io

exception Exists

let () = Callback.register_exception "ocaml_avfilter_exists" Exists
let failure message = raise (Error (`Failure message))

external sink_time_base : config -> instance -> rational
  = "ocaml_avfilter_sink_time_base"

external sink_frame_rate : config -> instance -> rational
  = "ocaml_avfilter_sink_frame_rate"

external sink_width : config -> instance -> int = "ocaml_avfilter_sink_width"
external sink_height : config -> instance -> int = "ocaml_avfilter_sink_height"

external sink_sample_aspect_ratio : config -> instance -> rational
  = "ocaml_avfilter_sink_sample_aspect_ratio"

external sink_format : config -> instance -> int = "ocaml_avfilter_sink_format"

external sink_channels : config -> instance -> int
  = "ocaml_avfilter_sink_channels"

external sink_sample_rate : config -> instance -> int
  = "ocaml_avfilter_sink_sample_rate"

external sink_channel_layout : config -> instance -> Channel_layout.t
  = "ocaml_avfilter_sink_channel_layout"

external sink_set_frame_size : config -> instance -> int -> unit
  = "ocaml_avfilter_set_frame_size"

external pixel_format_of_id : int -> Pixel_format.t
  = "ocaml_avfilter_pixel_format_of_id"

external sample_format_of_id : int -> Sample_format.t
  = "ocaml_avfilter_sample_format_of_id"

let time_base { graph; instance } = sink_time_base graph instance
let frame_rate { graph; instance } = sink_frame_rate graph instance
let width { graph; instance } = sink_width graph instance
let height { graph; instance } = sink_height graph instance

let pixel_aspect { graph; instance } =
  match sink_sample_aspect_ratio graph instance with
    | { num = 0; _ } -> None
    | ratio -> Some ratio

let pixel_format { graph; instance } =
  pixel_format_of_id (sink_format graph instance)

let channels { graph; instance } = sink_channels graph instance
let channel_layout { graph; instance } = sink_channel_layout graph instance
let sample_rate { graph; instance } = sink_sample_rate graph instance

let sample_format { graph; instance } =
  sample_format_of_id (sink_format graph instance)

let set_frame_size { graph; instance } size =
  sink_set_frame_size graph instance size

external array_separator : string -> string -> int
  = "ocaml_avfilter_array_separator"

let get_array_separator ~filter_name ~option_name =
  match array_separator filter_name option_name with
    | 0 -> ','
    | separator -> Char.chr separator

(* The pads of one direction as the C side gives them: (name, kind), kind
   being 0 for audio, 1 for video and 2 for anything else. *)
type raw_pads = (string * int) array
type raw_filter = string * string * Options.t * flag list * raw_pads * raw_pads

external registry : unit -> raw_filter array = "ocaml_avfilter_registry"

let pads_of_raw ~filter ~attachment (raw : raw_pads) =
  let of_kind kind =
    List.filter_map Fun.id
      (List.mapi
         (fun index (pad_name, pad_kind) ->
           if pad_kind = kind then
             Some { pad_name; pad_filter = filter; index; attachment }
           else None)
         (Array.to_list raw))
  in
  { audio = of_kind 0; video = of_kind 1 }

let filter_of_raw (name, description, options, flags, inputs, outputs) =
  let pads = pads_of_raw ~filter:name ~attachment:None in
  {
    name;
    description;
    options;
    flags;
    io = { inputs = pads inputs; outputs = pads outputs };
  }

let endpoints = ["abuffer"; "buffer"; "abuffersink"; "buffersink"]
let registered = List.map filter_of_raw (Array.to_list (registry ()))

let filters =
  List.filter (fun filter -> not (List.mem filter.name endpoints)) registered
  |> List.sort (fun a b -> String.compare a.name b.name)

let find_opt name = List.find_opt (fun filter -> filter.name = name) filters

let find name =
  match find_opt name with Some filter -> filter | None -> raise Not_found

let endpoint name = List.find (fun filter -> filter.name = name) registered
let abuffer = endpoint "abuffer"
let buffer = endpoint "buffer"
let abuffersink = endpoint "abuffersink"
let buffersink = endpoint "buffersink"
let pad_name pad = pad.pad_name
let filter_name pad = pad.pad_filter

external init : unit -> config = "ocaml_avfilter_init"

(* spec/avfilter.md §11.2. *)
let argument_string ~filter_name args =
  let ground : ground_arg -> string = function
    | `String s -> s
    | `Int i -> string_of_int i
    | `Int64 i -> Int64.to_string i
    | `Float f -> Printf.sprintf "%.17g" f
    | `Rational { num; den } -> Printf.sprintf "%d/%d" num den
  in
  let argument : args -> string = function
    | `Flag flag -> flag
    | `Pair (key, (#ground_arg as value)) -> key ^ "=" ^ ground value
    | `Pair (key, `Array values) ->
        let separator =
          String.make 1 (get_array_separator ~filter_name ~option_name:key)
        in
        key ^ "=" ^ String.concat separator (List.map ground values)
  in
  String.concat ":" (List.map argument args)

external attach_instance :
  config -> string -> string -> string -> instance * raw_pads * raw_pads
  = "ocaml_avfilter_attach"

let attach ?(args = []) ~name filter graph =
  let instance, inputs, outputs =
    attach_instance graph filter.name name
      (argument_string ~filter_name:filter.name args)
  in
  let pads =
    pads_of_raw ~filter:filter.name ~attachment:(Some { graph; instance })
  in
  { filter with io = { inputs = pads inputs; outputs = pads outputs } }

let attachment pad =
  match pad.attachment with
    | Some attachment -> attachment
    | None -> failure "the pad is not attached"

external link_pads : config -> instance -> int -> instance -> int -> unit
  = "ocaml_avfilter_link"

let link source destination =
  let source_filter = attachment source
  and destination_filter = attachment destination in
  if source_filter.graph != destination_filter.graph then
    failure "the pads belong to different graphs";
  link_pads source_filter.graph source_filter.instance source.index
    destination_filter.instance destination.index

type command_flag = [ `Fast ]

external process_instance_command :
  config -> instance -> string -> string -> command_flag list -> string
  = "ocaml_avfilter_process_command"

(* The filter record is public: the instance is found through its pads,
   which cannot be forged. *)
let filter_attachment filter =
  let attachments pads = List.filter_map (fun pad -> pad.attachment) pads in
  let { inputs; outputs } = filter.io in
  match
    attachments inputs.audio @ attachments inputs.video
    @ attachments outputs.audio @ attachments outputs.video
  with
    | [] -> failure "the filter has no attached pad"
    | first :: rest ->
        if List.exists (fun other -> other.instance != first.instance) rest then
          failure "the pads of the filter belong to several instances";
        first

let process_command ?(flags = []) ~cmd ?(arg = "") filter =
  let { graph; instance } = filter_attachment filter in
  process_instance_command graph instance cmd arg flags

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

external parse_description :
  config ->
  (string * instance * int) array ->
  (string * instance * int) array ->
  string ->
  unit = "ocaml_avfilter_parse"

let parse { inputs; outputs } description graph =
  let node { node_name; node_pad; _ } =
    let attached = attachment node_pad in
    if attached.graph != graph then failure "the pad belongs to another graph";
    (node_name, attached.instance, node_pad.index)
  in
  let nodes { audio; video } =
    Array.of_list (List.map node audio @ List.map node video)
  in
  parse_description graph (nodes inputs) (nodes outputs) description

(* The endpoint filters, in the order of the C side. *)
type role = Audio_source | Video_source | Audio_sink | Video_sink

external launch_graph : config -> (string * role * instance) array
  = "ocaml_avfilter_launch"

external push : config -> instance -> _ frame option -> unit
  = "ocaml_avfilter_push"

external pull : config -> instance -> _ frame = "ocaml_avfilter_pull"

let launch graph =
  let endpoints = Array.to_list (launch_graph graph) in
  let of_role role make =
    List.filter_map
      (fun (name, endpoint_role, instance) ->
        if endpoint_role = role then Some (name, make instance) else None)
      endpoints
  in
  let source instance = function
    | `Frame frame -> push graph instance (Some frame)
    | `Flush -> push graph instance None
  in
  let sink instance =
    { context = { graph; instance }; handler = (fun () -> pull graph instance) }
  in
  {
    inputs =
      {
        audio = of_role Audio_source source;
        video = of_role Video_source source;
      };
    outputs =
      { audio = of_role Audio_sink sink; video = of_role Video_sink sink };
  }

module Utils = struct
  type audio_params = {
    sample_rate : int;
    channel_layout : Avutil.Channel_layout.t;
    sample_format : Avutil.Sample_format.t;
  }

  type audio_converter = {
    source : [ `Audio ] input;
    sink : [ `Audio ] output;
    converter_time_base : rational;
  }

  let format_name sample_format =
    Option.value (Sample_format.get_name sample_format) ~default:"none"

  (* spec/avfilter.md §11.3. *)
  let init_audio_converter ?out_params ?out_frame_size ~in_time_base ~in_params
      () =
    let graph = init () in
    let source =
      attach ~name:"source"
        ~args:
          [
            `Pair ("sample_rate", `Int in_params.sample_rate);
            `Pair ("time_base", `Rational in_time_base);
            `Pair
              ( "channel_layout",
                `String
                  (Channel_layout.get_description in_params.channel_layout) );
            `Pair ("sample_fmt", `String (format_name in_params.sample_format));
          ]
        abuffer graph
    in
    let last =
      match out_params with
        | None -> source
        | Some out_params ->
            let aresample =
              match find_opt "aresample" with
                | Some filter -> filter
                | None -> raise (Error `Filter_not_found)
            in
            let resample =
              attach ~name:"resample"
                ~args:
                  [
                    `Pair ("out_sample_rate", `Int out_params.sample_rate);
                    `Pair
                      ( "out_chlayout",
                        `String
                          (Channel_layout.get_description
                             out_params.channel_layout) );
                    `Pair
                      ( "out_sample_fmt",
                        `String (format_name out_params.sample_format) );
                  ]
                aresample graph
            in
            link
              (List.hd source.io.outputs.audio)
              (List.hd resample.io.inputs.audio);
            resample
    in
    let sink = attach ~name:"sink" abuffersink graph in
    link (List.hd last.io.outputs.audio) (List.hd sink.io.inputs.audio);
    let endpoints = launch graph in
    let sink = List.assoc "sink" endpoints.outputs.audio in
    Option.iter (set_frame_size sink.context) out_frame_size;
    {
      source = List.assoc "source" endpoints.inputs.audio;
      sink;
      converter_time_base = time_base sink.context;
    }

  let time_base converter = converter.converter_time_base

  (* Only the sink's own answers end a delivery: what [deliver] raises
     passes through. *)
  let rec deliver converter callback =
    match converter.sink.handler () with
      | frame ->
          callback frame;
          deliver converter callback
      | exception Error (`Eagain | `Eof) -> ()

  let convert_audio converter callback input =
    deliver converter callback;
    converter.source input;
    deliver converter callback
end
