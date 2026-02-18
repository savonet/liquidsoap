type format = string

let audio_formats =
  [
    {|%fdkaac(aot="mpeg4_aac_lc",channels=1).aac|};
    "%fdkaac(channels=2).aac";
    "%shine(channels=1).mp3";
    "%shine(channels=2).mp3";
    "%flac(stereo).flac";
    "%flac(mono).flac";
    "%wav(stereo).wav";
    "%wav(mono).wav";
    "%mp3(mono).mp3";
    "%mp3(stereo).mp3";
    "%ogg(%vorbis(mono)).ogg";
    "%ogg(%vorbis(stereo)).ogg";
    "%ogg(%flac(mono)).ogg";
    "%ogg(%flac(stereo)).ogg";
    "%ogg(%opus(mono)).ogg";
    "%ogg(%opus(stereo)).ogg";
    {|%ffmpeg(format="mp4",%audio(codec="aac",samplerate="48k")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac")).mp4|};
    {|%ffmpeg(format="mp4",%audio(pcm_s16,codec="aac")).mp4|};
    {|%ffmpeg(format="mp4",%audio(pcm_f32,codec="aac")).mp4|};
  ]

let video_formats = [{|%ffmpeg(format="mp4",%video(codec="libx264")).mp4|}]

let audio_video_formats =
  [
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=1),%video(codec="libx264")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%video(codec="libx264")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%video(codec="libx264",r=12)).mp4|};
  ]

let multitrack_formats =
  [
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%audio_2(codec="aac",channels=1),%video(codec="libx264"),%video_2(codec="libx264")).mp4|};
  ]

let audio_subtitle_formats =
  [
    {|%ffmpeg(format="matroska",%audio(codec="aac"),%subtitles(codec="subrip")).mkv|};
    {|%ffmpeg(format="matroska",%audio(codec="aac"),%subtitles(codec="ass")).mkv|};
  ]

let video_subtitle_formats =
  [
    {|%ffmpeg(format="matroska",%video(codec="libx264"),%subtitles(codec="subrip")).mkv|};
    {|%ffmpeg(format="matroska",%video(codec="libx264",b="500k"),%subtitles(codec="ass")).mkv|};
    {|%ffmpeg(format="webm",%video(codec="libvpx"),%subtitles(codec="webvtt")).webm|};
  ]

let audio_video_subtitle_formats =
  [
    {|%ffmpeg(format="matroska",%audio(codec="aac"),%video(codec="libx264"),%subtitles(codec="subrip")).mkv|};
    {|%ffmpeg(format="matroska",%audio(codec="aac",b="128k"),%video(codec="libx264"),%subtitles(codec="ass")).mkv|};
    {|%ffmpeg(format="mp4",%audio(codec="aac"),%video(codec="libx264"),%subtitles(codec="mov_text")).mp4|};
  ]

let all =
  audio_formats @ audio_video_formats @ video_formats @ multitrack_formats
  @ audio_subtitle_formats @ video_subtitle_formats
  @ audio_video_subtitle_formats

let escaped_format =
  String.map (function
    | '%' -> '@'
    | '"' -> '\''
    | '(' -> '['
    | ')' -> ']'
    | c -> c)

let filename format = escaped_format format
