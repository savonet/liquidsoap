log.level.set(4)

# Test that add() relays metadata from all sources, not just the first one.

s1 = sine(duration=10.)
s2 = sine(duration=10.)

received_from_s1 = ref(false)
received_from_s2 = ref(false)

def check_metadata(m) =
  print(
    "Got metadata: #{m}"
  )
  if
    list.assoc.mem("source", m)
  then
    source_id = m["source"]
    if
      source_id == "s1"
    then
      received_from_s1 := true
    elsif source_id == "s2" then received_from_s2 := true
    end
    if
      received_from_s1() and received_from_s2()
    then
      print(
        "Received metadata from both sources!"
      )
      test.pass()
    end
  end
end

def insert_metadata1() =
  print(
    "Inserting metadata into s1"
  )
  s1.insert_metadata(
    [
      ("source", "s1"),
      (
        "title",
        "Track from s1"
      )
    ]
  )
end

def insert_metadata2() =
  print(
    "Inserting metadata into s2"
  )
  s2.insert_metadata(
    [
      ("source", "s2"),
      (
        "title",
        "Track from s2"
      )
    ]
  )
end

s = add(normalize=false, [s1, s2])
s.on_metadata(synchronous=true, check_metadata)

thread.run(delay=1., insert_metadata1)
thread.run(delay=2., insert_metadata2)
output.dummy(fallible=true, s)
