open Ctypes
include C.Functions

module LV2 = struct
  include LV2
end

module Node = struct
  include Node

  let finalise n = Gc.finalise free n

  let finalised n =
    finalise n;
    n

  let uri w s = finalised (uri w s)
  let string w s = finalised (string w s)
  let int w s = finalised (int w s)
  let float w s = finalised (float w s)
  let bool w s = finalised (bool w s)
end

module Nodes = struct
  include Nodes

  let length p = length p
  let iterate p : nodes_iterator = (p, iterate p)
  let get ((p, i) : nodes_iterator) = get p i
  let next ((p, i) : nodes_iterator) = (p, next p i)
  let is_end ((p, i) : nodes_iterator) = is_end p i

  let iter f p =
    let i = ref (iterate p) in
    while not (is_end !i) do
      f (get !i);
      i := next !i
    done

  let to_list p : Node.t list =
    let ans = ref [] in
    iter (fun p -> ans := p :: !ans) p;
    List.rev !ans
end

module Port = struct
  include Port

  let is_a n p = is_a (get_plugin p) (get_port p) n
  let is_input p = is_a (Node.uri (get_world p) LV2.Core.input_port) p
  let is_output p = is_a (Node.uri (get_world p) LV2.Core.output_port) p
  let is_audio p = is_a (Node.uri (get_world p) LV2.Core.audio_port) p
  let is_control p = is_a (Node.uri (get_world p) LV2.Core.control_port) p
  let has_property n p = has_property (get_plugin p) (get_port p) n

  let is_connection_optional p =
    has_property (Node.uri (get_world p) LV2.Core.connection_optional) p

  let index p = Unsigned.UInt32.to_int (index (get_plugin p) (get_port p))
  let symbol p = Node.to_string (symbol (get_plugin p) (get_port p))

  let name p =
    Node.to_string (Node.finalised (name (get_plugin p) (get_port p)))

  let range p =
    let def = allocate node Node.null in
    let min = allocate node Node.null in
    let max = allocate node Node.null in
    range (get_plugin p) (get_port p) def min max;
    let def = Node.finalised !@def in
    let min = Node.finalised !@min in
    let max = Node.finalised !@max in
    (def, min, max)

  let range_float p =
    let def, min, max = range p in
    (Node.to_float def, Node.to_float min, Node.to_float max)

  let default_float p =
    let def, _, _ = range p in
    let def = Node.to_float def in
    if compare def nan = 0 then None else Some def

  let min_float p =
    let _, min, _ = range p in
    let min = Node.to_float min in
    if compare min nan = 0 then None else Some min

  let max_float p =
    let _, _, max = range p in
    let max = Node.to_float max in
    if compare max nan = 0 then None else Some max
end

module Plugin = struct
  include Plugin

  let uri p = Node.to_uri (uri (get_plugin p))
  let name p = Node.to_string (Node.finalised (name (get_plugin p)))

  let author_name p =
    match author_name (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  let author_email p =
    match author_email (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  let author_homepage p =
    match author_homepage (get_plugin p) with
      | Some node -> Node.to_string (Node.finalised node)
      | None -> ""

  module Class = struct
    include Class

    let label c = Node.to_string (label c)
  end

  let plugin_class p = plugin_class (get_plugin p)
  let supported_features p = Nodes.to_list (supported_features (get_plugin p))
  let required_features p = Nodes.to_list (required_features (get_plugin p))
  let optional_features p = Nodes.to_list (optional_features (get_plugin p))
  let num_ports p = Int32.to_int (num_ports (get_plugin p))
  let is_replaced p = is_replaced (get_plugin p)

  let port_by_index p i =
    Port.make p (port_by_index (get_plugin p) (Int32.of_int i))

  let port_by_symbol p s =
    Port.make p (port_by_symbol (get_plugin p) (Node.string (get_world p) s))

  let has_latency p = has_latency (get_plugin p)
  let latency_port_index p = Int32.to_int (latency_port_index (get_plugin p))

  module Instance = struct
    include Instance

    let finalised i =
      Gc.finalise free i;
      i

    let connect_port (i : t) n =
      (* getf !@(descriptor i) LV2.descriptor_connect_port (handle i) (Unsigned.UInt32.of_int n) *)
      let f = getf !@(descriptor i) LV2.descriptor_connect_port in
      let f =
        Ctypes.coerce LV2.descriptor_connect_port_ptr_type
          (Foreign.funptr LV2.descriptor_connect_port_type)
          f
      in
      f (handle i) (Unsigned.UInt32.of_int n)

    let connect_port_float i n
        (data :
          (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t) =
      let data = array_of_bigarray array1 data in
      connect_port i n (to_voidp (CArray.start data))

    let activate i =
      let f = getf !@(descriptor i) LV2.descriptor_activate in
      let f =
        Ctypes.coerce LV2.descriptor_activate_ptr_type
          (Foreign.funptr LV2.descriptor_activate_type)
          f
      in
      f (handle i)

    let deactivate i =
      let f = getf !@(descriptor i) LV2.descriptor_deactivate in
      let f =
        Ctypes.coerce LV2.descriptor_deactivate_ptr_type
          (Foreign.funptr LV2.descriptor_deactivate_type)
          f
      in
      f (handle i)

    let run i n =
      let f = getf !@(descriptor i) LV2.descriptor_run in
      let f =
        Ctypes.coerce LV2.descriptor_run_ptr_type
          (Foreign.funptr LV2.descriptor_run_type)
          f
      in
      f (handle i) (Unsigned.UInt32.of_int n)
  end

  (* TODO: features *)
  let instantiate p samplerate =
    Instance.finalised
      (instantiate (get_plugin p) samplerate (from_voidp void null))
end

module Plugins = struct
  include Plugins

  let length p = length (get_plugins p)
  let iterate p : plugins_iterator = (p, iterate (get_plugins p))

  let get ((p, i) : plugins_iterator) =
    Plugin.make (get_world p) (get (get_plugins p) i)

  let get_by_uri p uri =
    match get_by_uri (get_plugins p) (Node.uri (get_world p) uri) with
      | Some plugin -> Plugin.make (get_world p) plugin
      | None -> raise Not_found

  let next ((p, i) : plugins_iterator) = (p, next (get_plugins p) i)
  let is_end ((p, i) : plugins_iterator) = is_end (get_plugins p) i

  let iter f p =
    let i = ref (iterate p) in
    while not (is_end !i) do
      f (get !i);
      i := next !i
    done

  let to_list p =
    let ans = ref [] in
    iter (fun p -> ans := p :: !ans) p;
    List.rev !ans
end

module State = struct
  include State
end

module World = struct
  include World

  let create () =
    let w = create () in
    Gc.finalise free w;
    w

  let plugins w = Plugins.make w (plugins w)
end
