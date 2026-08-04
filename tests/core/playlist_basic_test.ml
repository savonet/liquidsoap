let () =
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:,- songname"
    = [("song", "songname")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:,-songname"
    = [("song", "songname")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:,artist - title"
    = [("artist", "artist"); ("title", "title")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:,artist-title"
    = [("artist", "artist"); ("title", "title")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:123,- songname"
    = [("extinf_duration", "123"); ("song", "songname")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:123,-songname"
    = [("extinf_duration", "123"); ("song", "songname")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:123 ,artist - title"
    = [("extinf_duration", "123"); ("artist", "artist"); ("title", "title")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf "#EXTINF:123,artist-title"
    = [("extinf_duration", "123"); ("artist", "artist"); ("title", "title")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:123 foo=\"bla\",gni=\"gnu\",- songname"
    = [
        ("extinf_duration", "123");
        ("foo", "bla");
        ("gni", "gnu");
        ("song", "songname");
      ]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:123 foo=\"bla,-songname"
    = [("extinf_duration", "123")]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:123 foo=\"bla\",gni=\"gnu\",artist - title"
    = [
        ("extinf_duration", "123");
        ("foo", "bla");
        ("gni", "gnu");
        ("artist", "artist");
        ("title", "title");
      ]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:123 foo=\"bla\",gni=\"gnu\",artist-title"
    = [
        ("extinf_duration", "123");
        ("foo", "bla");
        ("gni", "gnu");
        ("artist", "artist");
        ("title", "title");
      ]);
  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:157,Blood, Sweat & Tears - Spinning wheel"
    = [
        ("extinf_duration", "157");
        ("artist", "Blood, Sweat & Tears");
        ("title", "Spinning wheel");
      ]);

  assert (
    Liquidsoap_builtins.Playlist_basic.parse_extinf
      "#EXTINF:157 foo=\"bla\",gni=\"gnu\",Blood, Sweat & Tears - Spinning \
       wheel"
    = [
        ("extinf_duration", "157");
        ("foo", "bla");
        ("gni", "gnu");
        ("artist", "Blood, Sweat & Tears");
        ("title", "Spinning wheel");
      ])
