# Enable ReplayGain metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `replaygain:` protocol.
# @category Liquidsoap
# @param ~delay Maximum delay for extracting metadata.
# @param ~extract_replaygain The extraction program.
def enable_replaygain_metadata(
       ~delay=(-1.))
  def replaygain_metadata(fname)
    gain = file.replaygain(fname)
    [("replaygain_track_gain","#{gain} dB")]
  end
  add_metadata_resolver("replaygain_track_gain", replaygain_metadata)
end

# @flag hidden
def youtube_playlist_parser(~pwd="",url) =
  ignore(pwd)
  binary = null.get(settings.protocol.youtube_dl.path())

  def parse_line(line) =
    let json.parse ( parsed  : {url : string}? ) = line
    parsed = parsed ?? {url = "foo"}
    url = parsed.url
    ([],"youtube-dl:#{url}")
  end

  if r/^youtube-pl/.test(url) then
    uri = list.nth(default="",r/:/.split(url), 1)
    list.map(parse_line, process.read.lines("#{binary} -j --flat-playlist #{uri}"))
  else
    []
  end
end
playlist.parse.register(format="youtube-dl",strict=true,youtube_playlist_parser)
