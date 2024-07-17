
(** Encode/decode WAV files. *)

type t

exception Not_a_wav_file of string

val fopen : string -> t
(** Open the named wav for reading, and return a new wav descriptor.
   Raise [Sys_error] if the file could not be opened and [Not_a_wav_file]
   if it hasn't the right format. *)

val sample : t -> string -> int -> int -> int
(** [sample w buf pos len] reads up to [len] characters from
   the given wav [w], storing them in string [buf], starting at
   character number [pos].
   It returns the actual number of characters read, between 0 and
   [len] (inclusive).
   A return value of 0 means that the end of file was reached.
   A return value between 0 and [len] exclusive means that
   not all requested [len] characters were read, either because
   no more characters were available at that time, or because
   the implementation found it convenient to do a partial read;
   [sample] must be called again to read the remaining characters,
   if desired.
   Exception [Invalid_argument "input"] is raised if [pos] and [len]
   do not designate a valid substring of [buf]. *)

val info : t -> string
(** [info w] returns a string containing some informations on wav [w] *)

val format : t -> Mixer.format
(** [format w] return the wav encoding format of [w] *)

val duration : t -> int
val remaining : t -> int
(** Not implemented yet, always return -1 *)

val close : t -> unit
(** [close w] close the wav descriptor [w] *)

val header : Mixer.format -> string
(** [header format] returns the WAV header that declares the given format.
  * The lengths of file and data are set to their maximum possible value. *)
