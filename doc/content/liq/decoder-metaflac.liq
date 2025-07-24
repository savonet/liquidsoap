if
  process.test(
    "which metaflac"
  )
then
  log(
    level=3,
    "Found metaflac binary: enabling flac external metadata resolver."
  )

  def flac_meta(~metadata:_, fname) =
    ret =
      process.read.lines(
        "metaflac --export-tags-to=- #{process.quote(fname)} 2>/dev/null"
      )
    ret = list.map(fun (s) -> string.split(separator="=", s), ret)

    # Could be made better.
    def f(l', l) =
      if
        list.length(l) >= 2
      then
        list.append([(list.hd(default="", l), list.nth(default="", l, 1))], l')
      else
        if
          list.length(l) >= 1
        then
          list.append([(list.hd(default="", l), "")], l')
        else
          l'
        end
      end
    end
    list.fold(f, [], ret)
  end

  decoder.metadata.add("FLAC", flac_meta)
end
