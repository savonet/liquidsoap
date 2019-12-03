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

open Lang_builtins

open Prometheus

let log = Log.make ["prometheus"]

let metric_proto = [
  "help",Lang.string_t,None,Some "Help of the metric";
  "namespace",Lang.string_t,Some (Lang.string ""),Some "namespace of the metric";
  "subsystem",Lang.string_t,Some (Lang.string ""),Some "subsystem of the metric";
  "labels",Lang.list_t Lang.string_t,None,Some "labels for the metric";
  "",Lang.string_t,None,Some "Name of the metric"
]

let set_t =
  Lang.fun_t [false,"",Lang.float_t] Lang.unit_t

let register_t =
  Lang.fun_t [false,"label_values",Lang.list_t Lang.string_t] set_t

let add_metric metric_name create register set =
  add_builtin ("prometheus." ^ metric_name)
    ~cat:Interaction
    ~descr:("Register a prometheus " ^ metric_name)
    metric_proto
    register_t
    (fun p ->
      let help =
        Lang.to_string (List.assoc "help" p)
      in
      let label_names =
        List.map Lang.to_string
          (Lang.to_list (List.assoc "labels" p))
      in
      let opt_v n =
        match Lang.to_string (List.assoc n p) with
          | s when s = "" -> None
          | v -> Some v
      in
      let namespace = opt_v "namespace" in
      let subsystem = opt_v "subsystem" in
      let name = Lang.to_string (List.assoc "" p) in
      let m =
        create ~label_names ?registry:None ~help ?namespace ?subsystem name
      in
      Lang.val_fun ["label_values","label_values",Lang.list_t Lang.string_t,None] ~ret_t:set_t
        (fun p _ ->
          let labels_v = List.assoc "label_values" p in
          let labels =
            List.map Lang.to_string (Lang.to_list labels_v)
          in
          if List.length labels <> List.length label_names then
            raise (Lang_errors.Invalid_value (labels_v,"Not enough labels provided!"));          
          let m = register m labels in
          Lang.val_fun ["","",Lang.float_t,None] ~ret_t:Lang.unit_t
            (fun p _ ->
              let v =
                Lang.to_float (List.assoc "" p)
              in
              set m v;
              Lang.unit)))

let () =
  add_metric
    "counter"
    Counter.v_labels
    Counter.labels
    Counter.inc;
  add_metric
    "gauge"
    Gauge.v_labels
    Gauge.labels
    Gauge.set;
  add_metric 
    "summary"
    Summary.v_labels
    Summary.labels
    Summary.observe

let latencies = Hashtbl.create 10

let get_latencies ~prefix ~label_names mode =
  let key =
    String.concat "" (mode::label_names)
  in
  match Hashtbl.find_opt latencies key with
    | Some l -> l
    | None ->
        let latency =
          Gauge.v_labels ~label_names
            ~help:(Printf.sprintf "Mean %s latency over the chosen window" mode)
           (Printf.sprintf "%s%s_latency_seconds" prefix mode)
        in
        let peak_latency =
          Prometheus.Gauge.v_labels ~label_names
            ~help:(Printf.sprintf "Peak %s latency over the chosen window" mode)
           (Printf.sprintf "%s%s_peak_latency_seconds" prefix mode)
        in
        let max_latency =
          Prometheus.Gauge.v_labels ~label_names
            ~help:(Printf.sprintf "Max %s latency since start" mode)
           (Printf.sprintf "%s%s_max_latency_seconds" prefix mode)
        in
        Hashtbl.add latencies key (latency,peak_latency,max_latency);
        (latency,peak_latency,max_latency)

let last_data = ref None

let get_last_data ~label_names =
  match !last_data with
    | Some m -> m
    | None ->
      let m =
        Gauge.v_labels ~label_names
          ~help:"Last time source produced some data."
          "liquidsoap_time_of_last_data_timestamp"
      in
      last_data := Some m;
      m

let source_monitor ~prefix ~label_names ~labels ~window s =
  let mean l =
    let n = Hashtbl.length l in
    if n = 0 then 0. else
    let s =
      Hashtbl.fold (fun _ v cur ->
        cur+.v) l 0.
    in
    s /. float_of_int n
  in
  let track_latency mode =
    let latency, peak_latency, max_latency =
      get_latencies ~prefix ~label_names mode
    in
    let latency =
      Prometheus.Gauge.labels latency labels
    in
    let peak_latency =
      Prometheus.Gauge.labels peak_latency labels
    in
    let max_latency =
      Prometheus.Gauge.labels max_latency labels
    in  
    let latencies = Hashtbl.create 100 in
    let max = ref (-1.) in
    let add_latency l =
      let t = Unix.gettimeofday() in
      Hashtbl.add latencies t l;
      Hashtbl.filter_map_inplace (fun old_t v ->
        if t-.window <= old_t then Some v else None) latencies;
      let peak = Hashtbl.fold (fun _ v cur ->
        if cur < v then v else cur) latencies 0.
      in
      if !max < peak then
        max := peak;
      Prometheus.Gauge.set latency (mean latencies);
      Prometheus.Gauge.set peak_latency peak;
      Prometheus.Gauge.set max_latency (!max)
    in
    add_latency
  in
  let frame_duration =
    Lazy.force Frame.duration
  in
  let add_input_latency = track_latency "input" in
  let add_output_latency =  track_latency "output" in
  let add_overall_latency = track_latency "overall" in
  let last_start_time = ref 0. in
  let last_end_time = ref 0. in
  let last_data =
    Gauge.labels (get_last_data ~label_names) labels
  in
  let get_ready ~stype:_ ~is_output:_ ~id:_ ~content_kind:_
                ~clock_id:_ ~clock_sync_mode:_ =
    ()
  in
  let leave () = () in
  let get_frame ~start_time ~end_time
                ~start_position ~end_position
                ~is_partial:_ ~metadata:_ =
    last_start_time := start_time;
    last_end_time := end_time;
    Prometheus.Gauge.set last_data end_time;
    let encoded_time =
      Frame.seconds_of_master (end_position-start_position)
    in
    let latency = (end_time -. start_time) /. encoded_time in
    add_input_latency latency
  in
  let after_output () =
    let current_time = Unix.gettimeofday () in
    add_output_latency  ((current_time-.(!last_end_time)) /. frame_duration);
    add_overall_latency ((current_time-.(!last_start_time)) /. frame_duration)
  in
  let watcher = {Source.
    get_ready;leave;get_frame;after_output
  } in
  s#add_watcher watcher

let () =
  let source_monitor_register_t =
    Lang.fun_t [
      false,"label_values",Lang.list_t Lang.string_t;
      false,"",Lang.source_t (Lang.univ_t ())
    ] Lang.unit_t
  in
  Lang_builtins.add_builtin "prometheus.latency"
    [ "window", Lang.float_t, Some (Lang.float 5.),
      Some "Window over which mean and peak metrics are reported.";
      "prefix", Lang.string_t, Some (Lang.string "liquidsoap_"),
      Some "Prefix for the metric's name";
      "labels",Lang.list_t Lang.string_t,None,Some "labels for the metric"]
    source_monitor_register_t
    ~cat:Lang_builtins.Liq
    ~descr:"Monitor a source's internal latencies on Prometheus"
    (fun p ->
      let window = Lang.to_float (List.assoc "window" p) in
      let prefix = Lang.to_string (List.assoc "prefix" p) in
      let label_names =
        List.map Lang.to_string
          (Lang.to_list (List.assoc "labels" p))
      in
      Lang.val_fun ["label_values","label_values",Lang.list_t Lang.string_t,None;
                    "","",Lang.source_t (Lang.univ_t ()), None] ~ret_t:Lang.unit_t
        (fun p _ ->
          let s = Lang.to_source (List.assoc "" p) in
          let labels_v = List.assoc "label_values" p in
          let labels =
            List.map Lang.to_string (Lang.to_list labels_v)
          in
          if List.length labels <> List.length label_names then
            raise (Lang_errors.Invalid_value (labels_v,"Not enough labels provided!"));
          source_monitor ~label_names ~labels ~window ~prefix s;
          Lang.unit))
