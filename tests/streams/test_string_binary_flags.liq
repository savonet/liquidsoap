cover_data = "fake cover art data"

s = sine()
s = metadata.map(insert_missing=true, (fun (_) -> [("apic", cover_data)]), s)

def on_meta(m) =
  cover = m["apic"]
  b = string.flags.binary(cover)
  if b() then test.pass() else test.fail() end
end

s.on_metadata(synchronous=true, on_meta)
output.dummy(s)
