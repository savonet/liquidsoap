def mount(path) =
  if
    path == "/live.mp3"
  then
    {
      source_user = "dj",
      source_password = "dj-password",
      max_listeners = 100,
      fallback = {mount = "/autodj.mp3"}
    }
  elsif
    string.contains(prefix="/test/", path)
  then
    {hidden = true, max_listener_duration = 600.}
  else
    null
  end
end

icecast.server({source_password = "change-me", mount = mount})
