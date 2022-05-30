type format
type encoder

val kind_of_format : format -> Frame.kind Frame.fields
val string_of_format : format -> string

type factory = string -> unit -> encoder

val get_factory : format -> factory
