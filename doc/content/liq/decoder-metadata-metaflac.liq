def metaflac_metadata(~metadata:_, fname) =
  lines =
    process.read.lines(
      "metaflac --export-tags-to=- #{process.quote(fname)}"
    )
  def parse_line(meta, line) =
    ret = string.extract(pattern="^([^=]+)=(.*)$", line)
    if
      list.length(ret) == 3
    then
      [(string.case(lower=true, ret[1]), ret[2]), ...meta]
    else
      meta
    end
  end
  list.fold(parse_line, [], lines)
end

decoder.metadata.add(file_extensions=["flac"], "metaflac", metaflac_metadata)
