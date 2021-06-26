# Enable ReplayGain metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `replaygain:` protocol.
# @category Liquidsoap
# @param ~delay Maximum delay for extracting metadata.
# @param ~extract_replaygain The extraction program.
def enable_replaygain_metadata(
       ~delay=(-1.),
       ~extract_replaygain="#{configure.bindir}/extract-replaygain")
  def replaygain_metadata(file)
    ret = exec_replaygain(delay=delay,extract_replaygain=extract_replaygain,file)
    if ret != "" then
      [("replaygain_track_gain",ret)]
    else
      []
    end
  end
  add_metadata_resolver("replaygain_track_gain", replaygain_metadata)
end

# @flag hidden
def youtube_playlist_parser(~pwd="",url) =
  binary = null.get(settings.protocol.youtube_dl.path())

  def parse_line(line) =
    parsed = of_json(default=[("url","foo")],line)
    url = list.assoc(default="","url",parsed)
    ([],"youtube-dl:#{url}")
  end

  if string.match(pattern="^youtube-pl:",url) then
    uri = list.nth(default="",string.split(separator=":",url),1)
    list.map(parse_line, process.read.lines("#{binary} -j --flat-playlist #{uri}"))
  else
    []
  end
end
add_playlist_parser(format="youtube-dl",strict=true,youtube_playlist_parser)
