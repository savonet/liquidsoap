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
