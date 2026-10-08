(** LV2 specification. *)
module LV2 : sig
  (** Core LV2 specification. *)
  module Core : sig
    val uri : string
    val prefix : string -> string
    val input_port : string
    val output_port : string
    val audio_port : string
    val control_port : string
    val connection_optional : string
  end
end

type world

(** Nodes *)
module Node : sig
  (** A node. *)
  type t

  val equals : t -> t -> bool

  (** NULL node. *)
  val null : t

  (** Create an URI node. *)
  val uri : world -> string -> t

  val is_uri : t -> bool

  (** Node as URI. *)
  val to_uri : t -> string

  val is_blank : t -> bool
  val to_blank : t -> string

  (** Node as string. *)
  val string : world -> string -> t

  val is_string : t -> bool

  (** Node as string. *)
  val to_string : t -> string

  val int : world -> int -> t
  val is_int : t -> bool
  val to_int : t -> int
  val float : world -> float -> t
  val is_float : t -> bool

  (** Node as float. *)
  val to_float : t -> float

  val bool : world -> bool -> t
  val is_bool : t -> bool
end

module Nodes : sig
  (** A collection of nodes. *)
  type t

  (** Length of the collection. *)
  val length : t -> int

  (** Iterate a function on a collection. *)
  val iter : (Node.t -> unit) -> t -> unit

  (** Convert collection to list. *)
  val to_list : t -> Node.t list
end

(** Ports. *)
module Port : sig
  (** A port. *)
  type t

  (** Determine if a port is of a given class (input, output, audio, etc). *)
  val is_a : Node.t -> t -> bool

  (** Whether a port is an input. *)
  val is_input : t -> bool

  (** Whether a port is an output. *)
  val is_output : t -> bool

  (** Whether a port is an audio port. *)
  val is_audio : t -> bool

  (** Whether a port is a control port. *)
  val is_control : t -> bool

  (** Whether a port is a control port. *)
  val has_property : Node.t -> t -> bool

  (** Whether connection to a port is optional. *)
  val is_connection_optional : t -> bool

  (** Index of a port. *)
  val index : t -> int

  (** Symbol of a port. *)
  val symbol : t -> string

  (** Name of a port. *)
  val name : t -> string

  (** Range of a port (default, minimal and maximal values). *)
  val range : t -> Node.t * Node.t * Node.t

  (** Range of a port as floats. *)
  val range_float : t -> float * float * float

  (** Default value of a port as float. *)
  val default_float : t -> float option

  (** Minimal value of a port as float. *)
  val min_float : t -> float option

  (** Maximal value of a port as float. *)
  val max_float : t -> float option
end

(** Plugins. *)
module Plugin : sig
  (** A plugin. *)
  type t

  (** URI of a plugin. *)
  val uri : t -> string

  (** Name of a plugin. *)
  val name : t -> string

  (** Author's name of a plugin. *)
  val author_name : t -> string

  (** Author's email of a plugin. *)
  val author_email : t -> string

  (** Author's homepage of a plugin. *)
  val author_homepage : t -> string

  (** Classes of plugins. *)
  module Class : sig
    (** A plugin class. *)
    type t

    (** Label of a plugin class. *)
    val label : t -> string
  end

  (** Class of a plugin. *)
  val plugin_class : t -> Class.t

  val is_replaced : t -> bool
  val supported_features : t -> Node.t list
  val required_features : t -> Node.t list
  val optional_features : t -> Node.t list

  (** Number of ports of a plugin. *)
  val num_ports : t -> int

  (** Retrieve a port by its index. *)
  val port_by_index : t -> int -> Port.t

  (** Retrieve a port by its symbol. *)
  val port_by_symbol : t -> string -> Port.t

  (** Whether or not the plugin introduces (and reports) latency. *)
  val has_latency : t -> bool

  (** Index of the plugin's latency port. It is a fatal error to call this on a
      plugin without checking if the port exists by first calling [has_latency].
      Any plugin that introduces unwanted latency that should be compensated for
      (by hosts with the ability/need) must provide this port, which is a
      control rate output port that reports the latency for each cycle in
      frames. *)
  val latency_port_index : t -> int

  (** Instances of plugins. *)
  module Instance : sig
    (** An instance of a plugin. *)
    type t

    (** Connect a port to float data. *)
    val connect_port_float :
      t ->
      int ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      unit

    (** Activate an instance. *)
    val activate : t -> unit

    (** Deactivate an instance. *)
    val deactivate : t -> unit

    (** Run an instance on given number of samples. *)
    val run : t -> int -> unit
  end

  (** Create an instance of a plugin. *)
  val instantiate : t -> float -> Instance.t
end

(** Collections of plugins. *)
module Plugins : sig
  (** A collection of plugins. *)
  type t

  (** Length of the collection. *)
  val length : t -> int

  (** Fin a plugin with given URI. *)
  val get_by_uri : t -> string -> Plugin.t

  (** Iterate a function on a collection. *)
  val iter : (Plugin.t -> unit) -> t -> unit

  (** Convert collection to list. *)
  val to_list : t -> Plugin.t list
end

(** Worlds. *)
module World : sig
  (** A world. *)
  type t = world

  (** Create a world. *)
  val create : unit -> world

  (** Load all plugins. *)
  val load_all : world -> unit

  (** Obtain the collection of all plugins. *)
  val plugins : world -> Plugins.t
end
