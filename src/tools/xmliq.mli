(* Generic xml parsing module
 * Uses xml-light library *)

type error = XmlError of string | Empty | UnknownType | Internal
exception Error of error

val string_of_error : error -> string

(* [get_tracks xml] returns a list of (metadatas,uri) 
 * of available tracks in the xml.
 * Currently known formats are:
 * - XSPF
 * - Podcast 
 * - SMIL 
 * - ASX *)
val tracks : string -> ((string * string) list * string) list
(* results formats are not checked, you may try them yourself.. *)

