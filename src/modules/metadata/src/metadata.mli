(** Read metadata from various file formats. *)

(** Functions for handling charset conversion. *)
module CharEncoding = MetadataCharEncoding

(** Guess the MIME type of a file. *)
module MIME : sig
  (** Guess the MIME type from file contents. Raises [Not_found] if none was
      found. *)
  val of_string : string -> string

  (** Same as [of_string] but takes a file name as argument. *)
  val of_file : string -> string
end

(** Generate metadata parsers given functions for converting charsets. *)
module Make : functor (_ : CharEncoding.T) -> sig
  (** Raised when the metadata is not valid. *)
  exception Invalid

  (** Metadata are represented as association lists (name, value). *)
  type metadata = (string * string) list

  (** Bigarray representation of (large) tags. *)
  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** When used, a custom parser can override the default parsing mechanism. It
      is passed the metadata label (without normalization), the expected length
      of the data, a regular read function an an optional bigarray read
      function. The custom parser can call any of the read function to get the
      corresponding tag's value. After doing so, the tag is ignored by the
      regular parsing process.

      Currently only supported for: ID3v2, MP4 and [metadata_block_picture] in
      FLAC metadata. *)
  type parser_handler = MetadataBase.parser_handler = {
    label : string;
    length : int;
    read : unit -> string;
    read_ba : (unit -> bigarray) option;
    skip : unit -> unit;
  }

  (** A custom parser, see [parser_handler]. *)
  type custom_parser = parser_handler -> unit

  (** Abstractions for reading from various sources. *)
  module Reader : sig
    (** A function to read taking the buffer to fill the offset and the length
        and returning the number of bytes actually read. *)
    type t = MetadataBase.Reader.t = {
      read : bytes -> int -> int -> int;
      read_ba : (int -> MetadataBase.bigarray) option;
      custom_parser : custom_parser option;
      seek : int -> unit;
      size : unit -> int option;
      reset : unit -> unit;
    }

    (** Go back at the beginning of the stream. *)
    val reset : t -> unit

    (** Specialize a parser to operate on files. *)
    val with_file :
      ?custom_parser:custom_parser -> (t -> metadata) -> string -> metadata

    (** Specialize a parser to operate on strings. *)
    val with_string :
      ?custom_parser:custom_parser -> (t -> metadata) -> string -> metadata
  end

  (** ID3v1 metadata.*)
  module ID3v1 = MetadataID3v1

  (** ID3v2 metadata. *)
  module ID3v2 = MetadataID3v2

  (** OGG metadata. *)
  module OGG = MetadataOGG

  (** Flac metadata. *)
  module FLAC = MetadataFLAC

  (** Jpeg metadata. *)
  module JPEG = MetadataJPEG

  (** PNG metadata. *)
  module PNG = MetadataPNG

  (** AVI metadata. *)
  module AVI = MetadataAVI

  (** MP4 metadata. *)
  module MP4 = MetadataMP4

  (** WAV metadata. *)
  module WAV = MetadataWAV

  (** RIFF metadata. *)
  module RIFF = MetadataRIFF

  (** Convert the charset encoding of a string. *)
  val recode :
    ?source:[ `ISO_8859_1 | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ] ->
    ?target:[ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ] ->
    string ->
    string

  (** ID3v1 and ID3v2 metadata. *)
  module ID3 : sig
    val parse : Reader.t -> (string * string) list

    val parse_file :
      ?custom_parser:custom_parser -> string -> (string * string) list
  end

  (** Return the first application which does not raise invalid. *)
  val first_valid : (Reader.t -> metadata) list -> Reader.t -> metadata

  (** Audio file formats. *)
  module Audio : sig
    val parse : Reader.t -> MetadataBase.metadata

    val parse_file :
      ?custom_parser:custom_parser -> string -> MetadataBase.metadata
  end

  (** Image file formats. *)
  module Image : sig
    val parse : Reader.t -> MetadataBase.metadata

    val parse_file :
      ?custom_parser:custom_parser -> string -> MetadataBase.metadata
  end

  (** Video file formats. *)
  module Video : sig
    val parse : Reader.t -> MetadataBase.metadata

    val parse_file :
      ?custom_parser:custom_parser -> string -> MetadataBase.metadata
  end

  (** All supported file formats. *)
  module Any : sig
    (** Generic metadata parsing. *)
    val parse : Reader.t -> MetadataBase.metadata

    (** Parse the metadata from a file. *)
    val parse_file :
      ?custom_parser:custom_parser -> string -> MetadataBase.metadata

    (** Parse the metadata from a string containing the contents of a file. *)
    val parse_string :
      ?custom_parser:custom_parser -> string -> MetadataBase.metadata
  end

  include module type of Any
end

include module type of Make (CharEncoding.Naive)
