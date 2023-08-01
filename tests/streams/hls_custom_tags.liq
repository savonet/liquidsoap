s = sine()

s = mksafe(s)

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

main_tags = ref(false)
aac_flags = ref(false)
mp4_flags = ref(false)
aac_insert_flags = ref(false)

def on_file_change(~state, p) =
  fname = path.basename(p)
  if state == "created" or state == "updated" then
    if fname == "main.m3u8" and
       string.contains(substring="X-CUSTOM", file.contents(p)) then
      main_tags := true
    end

    if fname == "aac.m3u8" then
      contents = file.contents(p)

      if string.contains(substring="X-CUSTOM", contents) then
        aac_flags := true
      end

      if string.contains(substring="X-CUSTOM-AAC-INSERT", contents) then
        aac_insert_flags := true
      end
    end

    if fname == "mp4.m3u8" then
      contents = file.contents(p)

      if string.contains(substring="X-CUSTOM", contents) then
        mp4_flags := true
      end
    end
  end

  if main_tags() and aac_flags() and
     mp4_flags() and aac_insert_flags() then
    test.pass()
  end
end

o = output.file.hls(
  playlist="main.m3u8",
  extra_tags=["X-CUSTOM \n"],
  on_file_change=on_file_change,
  tmp_dir,
  [
   ("aac", %ffmpeg(format="adts",%audio(codec="aac")).{extra_tags=["X-CUSTOM"]}),
   ("mp4", %ffmpeg(format="mp4",frag_duration=10,movflags="+dash+skip_sidx+skip_trailer+frag_custom",%audio(codec="aac")).{extra_tags=["X-CUSTOM"]})
  ],
  s
)

thread.run(delay=2., {list.hd(o.streams()).insert_tag("X-CUSTOM-AAC-INSERT")})


clock.assign_new(sync="none",[s])
