stereotool_lib =
  if
    file.exists("./libStereoTool_64.dylib")
  then
    "./libStereoTool_64.dylib"
  elsif
    file.exists("./libStereoTool_intel64.so")
  then
    "./libStereoTool_intel64.so"
  else
    ""
  end

if stereotool_lib == "" then test.skip() end

s = sine()
s.on_frame(synchronous=true, test.pass)

s = stereotool(library_file=stereotool_lib, active=true, s)

s = fallback([blank(), s])

output.dummy(s)
