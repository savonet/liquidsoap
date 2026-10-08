(** Registers FFmpeg's capture and playback devices with libavformat, for the
    whole process. Loading this module already does it: the call exists so that
    a program can name something from this library and make the linker keep it.

    Once registered, the device formats are among the formats
    [Av.Format.find_input_format] and [Av.Format.guess_output_format] find. A
    device is opened as a container with [Av.open_input] or [Av.open_output],
    the device as the URL, or with [Av.open_output_format], and takes its
    options through [?opts] of those functions. Which devices exist depends on
    the FFmpeg version, build and platform.

    It raises nothing, holds the runtime lock, and may be called from any thread
    at any time. *)
val init : unit -> unit
