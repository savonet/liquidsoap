music = chop(every=1., metadata=[("title", "music")], sine(amplitude=0.1, 440.))

jingles = chop(every=1., metadata=[("title", "jingle")], sine(amplitude=0.1, 220.))

def next(_) =
  sequence(merge=true, [jingles, jingles, source.fail()])
end

s = append(music, next)

meta_seen = ref([])

s.on_metadata(
  synchronous=true,
  fun (m) -> begin
    meta_seen := [...meta_seen(), m["title"]]

    if list.length(meta_seen()) == 9 then
      test.equal(meta_seen(), ["music", "jingle", "jingle", "music", "jingle", "jingle", "music", "jingle", "jingle",])
      test.pass()
   end
  end
)

clock.assign_new(sync='none', [s])

output.dummy(s)
