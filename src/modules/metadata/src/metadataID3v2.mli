(** Parse the ID3v2 header. *)
val parse :
  ?recode:MetadataCharEncoding.recode ->
  MetadataBase.Reader.t ->
  MetadataBase.metadata

(** Parse the ID3v2 header from a file. *)
val parse_file :
  ?recode:MetadataCharEncoding.recode ->
  ?custom_parser:MetadataBase.custom_parser ->
  string ->
  MetadataBase.metadata

(** Dump the ID3v2 header. *)
val dump : MetadataBase.Reader.t -> string

(** Dump the ID3v2 header from a file. *)
val dump_file : string -> string

type apic = {
  mime : string;
  picture_type : int;
  description : string;
  data : string;
}

type pic = {
  pic_format : string;
  pic_type : int;
  pic_description : string;
  pic_data : string;
}

(** Parse an APIC tag (containing album art). *)
val parse_apic : ?recode:MetadataCharEncoding.recode -> string -> apic

(** Parse a PIC tag (containing album art). *)
val parse_pic : ?recode:MetadataCharEncoding.recode -> string -> pic

(** Frame identifier. *)
type frame_id =
  [ `AENC
  | `APIC
  | `COMM
  | `COMR
  | `ENCR
  | `EQUA
  | `ETCO
  | `GEOB
  | `GRID
  | `IPLS
  | `LINK
  | `MCDI
  | `MLLT
  | `OWNE
  | `PCNT
  | `POPM
  | `POSS
  | `PRIV
  | `RBUF
  | `RVAD
  | `RVRB
  | `SYLT
  | `SYTC
  | `TALB
  | `TBPM
  | `TCOM
  | `TCON
  | `TCOP
  | `TDAT
  | `TDLY
  | `TENC
  | `TEXT
  | `TFLT
  | `TIME
  | `TIT1
  | `TIT2
  | `TIT3
  | `TKEY
  | `TLAN
  | `TLEN
  | `TMED
  | `TOAL
  | `TOFN
  | `TOLY
  | `TOPE
  | `TORY
  | `TOWN
  | `TPE1
  | `TPE2
  | `TPE3
  | `TPE4
  | `TPOS
  | `TPUB
  | `TRCK
  | `TRDA
  | `TRSN
  | `TRSO
  | `TSIZ
  | `TSRC
  | `TSSE
  | `TXXX
  | `TYER
  | `UFID
  | `USER
  | `USLT
  | `WCOM
  | `WCOP
  | `WOAF
  | `WOAR
  | `WOAS
  | `WORS
  | `WPAY
  | `WPUB
  | `WXXX ]

(** String representation of a frame identifier. *)
val string_of_frame_id : frame_id -> string

(** Parse a string into a frame id. *)
val frame_id_of_string : string -> frame_id option

(** Does a frame contain binary data? *)
val binary_frame : frame_id -> bool

(** Charset for encoding text. *)
type text_encoding = [ `ISO_8859_1 | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

(** Data contained in a frame. *)
type frame_data = [ `Text of text_encoding * string | `Binary of string ]

type frame_flag =
  [ `File_alter_preservation of bool | `Tag_alter_perservation of bool ]

(** Default flags for a frame. *)
val default_flags : frame_id -> frame_flag list

(** A ID3 frame. *)
type frame = { id : frame_id; data : frame_data; flags : frame_flag list }

(** Create an ID3v2 header. Consistency between [frame_id] and [frame_data] is
    not enforced and left to the user to check. *)
val make : version:int -> frame list -> string
