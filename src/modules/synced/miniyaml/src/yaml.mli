(** A minimal YAML reader and writer.

    The supported subset is the one people hand-write in configuration files:
    block mappings and sequences, inline flow collections ([[1, 2]] and
    [{a: b}]), plain and quoted scalars, and comments. Block scalars ([|] and
    [>]), anchors, aliases, tags and multi-document streams are rejected with an
    explicit error, and flow collections have to fit on a single line. *)

(** A YAML value. *)
type t =
  | Null  (** the null value, written [null] or [~] *)
  | Bool of bool
  | Float of float  (** any number, integers included *)
  | String of string
  | List of t list  (** a sequence, e.g. [- 1] *)
  | Assoc of (string * t) list
      (** a mapping, e.g. [a: 1]; entries are kept in the order they occur and
          duplicate keys are preserved *)

(** Parse a YAML document. On failure, the message is prefixed with the line at
    which the error was detected. *)
val of_string : string -> (t, string) result

(** Parse the YAML document contained in a file. Raises [Sys_error] if the file
    cannot be read. *)
val of_file : string -> (t, string) result

(** Print a YAML document, using block style and two-space indentation. The
    result always ends with a newline and can be read back with {!of_string}:
    [of_string (to_string v) = Ok v] for every [v]. *)
val to_string : t -> string

(** Write a YAML document to a file, as printed by {!to_string}. Raises
    [Sys_error] if the file cannot be written. *)
val to_file : string -> t -> unit
