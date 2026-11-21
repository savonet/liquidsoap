%include "../test.liq"

# A failed external decode must not leave its output file behind.
enable_external_ffmpeg_decoder(
  binary="false",
  mimes=[],
  file_extensions=["decodecheck5668"]
)

def outputs() =
  probe = file.temp("probe", "")
  file.remove(probe)
  list.filter(
    fun (f) ->
      string.sub(f, start=0, length=6) == "ffmpeg"
      and file.extension(f) == ".wav",
    file.ls(path.dirname(probe))
  )
end

def check() =
  encoded = file.temp(cleanup=true, "input", ".decodecheck5668")
  before = list.length(outputs())
  ignore(request.resolve(request.create(encoded)))
  test.equal(list.length(outputs()), before)
  test.pass()
end

test.check(check)
