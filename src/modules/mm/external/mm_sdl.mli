open Mm_video
open Mm_midi

val init : unit -> unit

class writer_to_screen : int -> int -> Video.IO.Writer.t
class midi_keyboard : MIDI.IO.Reader.t
