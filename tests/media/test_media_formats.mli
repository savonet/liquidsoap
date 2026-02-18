type format = string

val audio_formats : format list
val video_formats : format list
val audio_video_formats : format list
val multitrack_formats : format list
val audio_subtitle_formats : format list
val video_subtitle_formats : format list
val audio_video_subtitle_formats : format list
val all : format list
val escaped_format : format -> string
val filename : format -> string
