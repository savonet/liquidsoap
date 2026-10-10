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

type raw_filter = {
  raw_name : string;
  raw_description : string;
  raw_options : Options.t;
  raw_flags : flag list;
  raw_inputs : raw_pads;
  raw_outputs : raw_pads;
}

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

let filter_of_raw raw =
  let pads = pads_of_raw ~filter:raw.raw_name ~attachment:None in
  {
    name = raw.raw_name;
    description = raw.raw_description;
    options = raw.raw_options;
    flags = raw.raw_flags;
    io = { inputs = pads raw.raw_inputs; outputs = pads raw.raw_outputs };
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

  type filter_spec = string * args list

  let filter_of_transform : Display_matrix.transform -> filter_spec = function
    | `Transpose direction ->
        let direction =
          match direction with
            | `Clock -> "clock"
            | `Cclock -> "cclock"
            | `Clock_flip -> "clock_flip"
            | `Cclock_flip -> "cclock_flip"
        in
        ("transpose", [`Pair ("dir", `String direction)])
    | `Hflip -> ("hflip", [])
    | `Vflip -> ("vflip", [])
    | `Rotate angle ->
        ("rotate", [`Pair ("angle", `String (Printf.sprintf "%f*PI/180" angle))])

  let filter_of_cropping { top; bottom; left; right } : filter_spec option =
    if top = 0 && bottom = 0 && left = 0 && right = 0 then None
    else
      Some
        ( "crop",
          [
            `Pair ("w", `String (Printf.sprintf "iw-%d-%d" left right));
            `Pair ("h", `String (Printf.sprintf "ih-%d-%d" top bottom));
            `Pair ("x", `Int left);
            `Pair ("y", `Int top);
          ] )

  type display_layout = {
    width : int;
    height : int;
    pixel_aspect : rational option;
    filters : filter_spec list;
  }

  type undecided = [ `Invalid_cropping of cropping | `Odd_rotation of float ]

  let cropped ~width ~height ({ top; bottom; left; right } as cropping) =
    if
      top < 0 || bottom < 0 || left < 0 || right < 0
      || left + right >= width
      || top + bottom >= height
    then Result.Error (`Invalid_cropping cropping)
    else Ok (width - left - right, height - top - bottom)

  let transposed layout =
    {
      layout with
      width = layout.height;
      height = layout.width;
      pixel_aspect =
        Option.map
          (fun { num; den } -> { num = den; den = num })
          layout.pixel_aspect;
    }

  let transformed layout (transform : Display_matrix.transform) =
    match transform with
      | `Rotate angle -> Result.Error (`Odd_rotation angle)
      | `Hflip | `Vflip -> Ok layout
      | `Transpose _ -> Ok (transposed layout)

  (* spec/avfilter.md §11.4. *)
  let display_layout ?cropping ?display_matrix ?pixel_aspect ~width ~height () =
    let ( let* ) = Result.bind in
    let layout =
      let* width, height =
        match cropping with
          | None -> Ok (width, height)
          | Some cropping -> cropped ~width ~height cropping
      in
      let transforms =
        Option.fold ~none:[] ~some:Display_matrix.transforms display_matrix
      in
      let* layout =
        List.fold_left
          (fun layout transform ->
            Result.bind layout (fun l -> transformed l transform))
          (Ok { width; height; pixel_aspect; filters = [] })
          transforms
      in
      Ok
        {
          layout with
          filters =
            Option.to_list (Option.bind cropping filter_of_cropping)
            @ List.map filter_of_transform transforms;
        }
    in
    match layout with
      | Ok layout -> `Layout layout
      | Error (#undecided as undecided) -> `Undecided undecided

  type undecided_frame = [ undecided | `Hardware_frame ]

  let string_of_undecided : undecided_frame -> string = function
    | `Hardware_frame -> "the frame is in a hardware pixel format"
    | `Odd_rotation angle ->
        Printf.sprintf "the rotation of %g degrees is not a quarter turn" angle
    | `Invalid_cropping { top; bottom; left; right } ->
        Printf.sprintf
          "the cropping of %d, %d, %d and %d pixels (top, bottom, left, right) \
           does not fit the picture"
          top bottom left right

  let video_buffer_args ~time_base (format : Video.frame_format) : args list =
    [
      `Pair
        ( "video_size",
          `String (Printf.sprintf "%dx%d" format.width format.height) );
      `Pair ("pix_fmt", `Int (Pixel_format.get_id format.pixel_format));
      `Pair ("time_base", `Rational time_base);
    ]
    @ (match format.pixel_aspect with
      | None -> []
      | Some pixel_aspect -> [`Pair ("pixel_aspect", `Rational pixel_aspect)])
    @ (match format.color_space with
      | `Unspecified -> []
      | color_space ->
          [`Pair ("colorspace", `String (Color_space.name color_space))])
    @
      match format.color_range with
      | `Unspecified -> []
      | color_range -> [`Pair ("range", `String (Color_range.name color_range))]

  type display_chain = {
    format : Video.frame_format;
    graph_filters : filter_spec list;
  }

  type display_graph = {
    chain : display_chain;
    display_source : [ `Video ] input;
    display_sink : [ `Video ] output;
  }

  type decision = {
    undecided : undecided_frame;
    decided : filter_spec list option;
  }

  type display_converter = {
    cropping : cropping option;
    ignore : Video.frame_property list;
    display_time_base : rational;
    on_undecided : undecided_frame -> filter_spec list option;
    mutable last_decision : decision option;
    mutable display_graph : display_graph option;
    mutable unbuildable : display_chain option;
  }

  let warn_undecided undecided =
    Log.log `Warning
      (Printf.sprintf "Video left as stored, with its display matrix: %s."
         (string_of_undecided undecided));
    None

  let init_display_converter ?cropping ?(on_undecided = warn_undecided)
      ?(ignore = []) ~time_base () =
    {
      cropping;
      ignore;
      display_time_base = time_base;
      on_undecided;
      last_decision = None;
      display_graph = None;
      unbuildable = None;
    }

  let is_hardware pixel_format =
    List.mem `Hwaccel (Pixel_format.descriptor pixel_format).flags

  let frame_display_matrix frame =
    Frame_side_data.display_matrix
      (Option.to_list (Frame.find_side_data frame `Displaymatrix))

  (* The caller is asked once per run of frames with the same question. *)
  let decide converter undecided =
    match converter.last_decision with
      | Some decision when decision.undecided = undecided -> decision.decided
      | _ ->
          let decided = converter.on_undecided undecided in
          converter.last_decision <- Some { undecided; decided };
          decided

  (* [None] when nothing is decided for the frame. *)
  let frame_filters ~(format : Video.frame_format) ~display_matrix converter =
    if is_hardware format.pixel_format then decide converter `Hardware_frame
    else (
      match
        display_layout ?cropping:converter.cropping ?display_matrix
          ~width:format.width ~height:format.height ()
      with
        | `Layout { filters; _ } -> Some filters
        | `Undecided undecided ->
            decide converter (undecided :> undecided_frame))

  let build_display_graph ~time_base chain =
    let graph = init () in
    let source =
      attach ~name:"source"
        ~args:(video_buffer_args ~time_base chain.format)
        buffer graph
    in
    let last =
      List.fold_left
        (fun (index, previous) (name, args) ->
          let filter =
            match find_opt name with
              | Some filter -> filter
              | None -> raise (Error `Filter_not_found)
          in
          let filter =
            attach ~name:(Printf.sprintf "display%d" index) ~args filter graph
          in
          link
            (List.hd previous.io.outputs.video)
            (List.hd filter.io.inputs.video);
          (index + 1, filter))
        (0, source) chain.graph_filters
      |> snd
    in
    let sink = attach ~name:"sink" buffersink graph in
    link (List.hd last.io.outputs.video) (List.hd sink.io.inputs.video);
    let endpoints = launch graph in
    {
      chain;
      display_source = List.assoc "source" endpoints.inputs.video;
      display_sink = List.assoc "sink" endpoints.outputs.video;
    }

  let rec deliver_display graph callback =
    match graph.display_sink.handler () with
      | frame ->
          Frame.remove_side_data frame `Displaymatrix;
          callback frame;
          deliver_display graph callback
      | exception Error (`Eagain | `Eof) -> ()

  let flush_display converter callback =
    Option.iter
      (fun graph ->
        converter.display_graph <- None;
        graph.display_source `Flush;
        deliver_display graph callback)
      converter.display_graph

  let without_display_matrix frame =
    let frame = Frame.dup frame in
    Frame.remove_side_data frame `Displaymatrix;
    frame

  let same_chain converter chain chain' =
    Video.same_frame_format ~ignore:converter.ignore chain.format chain'.format
    && chain.graph_filters = chain'.graph_filters

  (* [None] when the chain cannot be built, which is remembered: the attempt is
     made once per run of frames asking for it. *)
  let display_graph converter callback chain =
    match (converter.display_graph, converter.unbuildable) with
      | Some graph, _ when same_chain converter graph.chain chain -> Some graph
      | _, Some unbuildable when same_chain converter unbuildable chain ->
          flush_display converter callback;
          None
      | _ -> (
          flush_display converter callback;
          match
            build_display_graph ~time_base:converter.display_time_base chain
          with
            | graph ->
                converter.display_graph <- Some graph;
                Some graph
            | exception Error error ->
                Log.log `Warning
                  (Printf.sprintf
                     "Video left as stored, with its display matrix: its \
                      filters cannot be built (%s)."
                     (string_of_error error));
                converter.unbuildable <- Some chain;
                None)

  (* spec/avfilter.md §11.5. *)
  let convert_display converter callback = function
    | `Flush -> flush_display converter callback
    | `Frame frame ->
        let display_matrix = frame_display_matrix frame in
        if display_matrix = None && converter.cropping = None then (
          flush_display converter callback;
          callback frame)
        else (
          let format = Video.frame_format frame in
          match frame_filters ~format ~display_matrix converter with
            | None ->
                flush_display converter callback;
                callback frame
            | Some [] ->
                flush_display converter callback;
                callback
                  (if display_matrix = None then frame
                   else without_display_matrix frame)
            | Some graph_filters -> (
                match
                  display_graph converter callback { format; graph_filters }
                with
                  | None -> callback frame
                  | Some graph ->
                      graph.display_source (`Frame frame);
                      deliver_display graph callback))
end
