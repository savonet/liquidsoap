radio = mksafe(playlist("~/music"))

# BEGIN
dir = "/var/www/hls"

def segment_name(s) =
  "#{s.stream_name}/#{s.position}.#{s.extname}"
end

o =
  output.file.hls(
    dir,
    segment_name=segment_name,
    perm=0o644,
    dir_perm=0o755,
    temp_dir="/var/www/.hls-tmp",
    [("aac", %fdkaac(bitrate = 128))],
    radio
  )

def sync(change) =
  name = string.residual(prefix="#{dir}/", change.path) ?? change.path
  if
    change.state == "deleted"
  then
    ignore(
      process.run(
        "rclone deletefile cdn:live/#{name}"
      )
    )
  else
    ignore(
      process.run(
        "rclone copyto #{change.path} cdn:live/#{name}"
      )
    )
  end
end

o.on_file_change(synchronous=true, fun (change) -> thread.run({sync(change)}))
# END
