# @flag hidden
def exec_replaygain(~extract_replaygain="#{configure.bindir}/extract-replaygain",
                       ~delay,file)
  log = log.info(label="extract.replaygain")
  p = process.read("#{extract_replaygain} #{string.quote(file)}")
  stdout = string.replace(pattern='\\n', fun (_) -> "", p.stdout)
  if p.status == "exit" then
    stdout
  else
    stderr = string.replace(pattern='\\n', fun (_) -> "", p.stderr)
    log("Replaygain extraction by #{extract_replaygain} failed with: #{stderr}")
    ""
  end
end

# Enable replay gain metadata resolver. This resolver will process any file
# decoded by liquidsoap and add a replay_gain metadata when this value could be
# computed. For a finer-grained replay gain processing, use the replay_gain
# protocol.
# @category Liquidsoap
# @param ~delay Maximum delay for extracting metadata
# @param ~extract_replaygain The extraction program
def enable_replaygain_metadata(
       ~delay=(-1.),
       ~extract_replaygain="#{configure.bindir}/extract-replaygain")
  def replaygain_metadata(file)
    ret = exec_replaygain(delay=delay,extract_replaygain=extract_replaygain,file)
    if ret != "" then
      [("replay_gain",ret)]
    else
      []
    end
  end
  add_metadata_resolver("replay_gain", replaygain_metadata)
end

# @flag hidden
def youtube_playlist_parser(~pwd="",url) =
  binary = get(default="youtube-dl","protocol.youtube-dl.path")

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
