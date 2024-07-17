# Two icecast outputs, one for low bandwidth
set outputs = [ "geek.ogg" ; "geek.modem.ogg" ]

set geek.ogg.host = "sparc8"

set geek.modem.ogg.host = "sparc8"
set geek.modem.ogg.bitrate = 32
set geek.modem.ogg.stereo = true
set geek.modem.ogg.sample_freq = 22050

# Telnet source for receiving some user requests
let req = telnet in
# Some file to fill the stream when everything else isn't ready
let on_fail = one_file "/some/default/file.ogg" in

# Many playlists, which are re-read every hour
let def = playlist ~reload_mode:"seconds" ~reload:3600 "/home/dbaelde/playlist.local" in
let ftp = playlist ~reload_mode:"seconds" ~reload:3600 "/home/dbaelde/playlist" in
let techno = playlist ~reload_mode:"seconds" ~reload:3600 "/home/dbaelde/playlist.techno" in
let jazz = playlist ~reload_mode:"seconds" ~reload:3600 "/home/nguenot/playlist.jazz" in
let rock = playlist ~reload_mode:"seconds" ~reload:3600 "/home/vsiles/playlist.rock" in
let oldstyle = playlist ~reload_mode:"seconds" ~reload:3600 "/home/nguenot/playlist.oldstyle" in
let smb = playlist ~reload_mode:"seconds" ~reload:3600 "smb://footwar/mp3/playlist" in

# A special playlist with jingles
let jingle = playlist ~reload_mode:"seconds" ~reload:3600 "/home/dbaelde/playlist.jingle" in
let jingle = change_volume 1.2 jingle in

# And some clock beep
let beep = sine 880 in

 add_w [
        # Add a "beep beep" around 0m.
        ((date_switch [("59m & 58s", beep);("0m & 0s", beep)]),3) ;

        # Playlists.
        ((random_w [
                # About one jingle every 9 tracks.
                (jingle,1) ;
                ((fallback [
                        # User requests have the priority.
                        req ;
                        # Otherwise, depends on the time.
                        (date_switch [("0-3h", techno)]) ;
                        (date_switch [("18-19h", jazz)]) ;
                        (date_switch [("8-9h", oldstyle)]) ;
                        (date_switch [("22-23h",rock)]);
                        # If nothing special planned, play our huge (remote) playlist.
                        smb ;
                        # Or a smaller but safer local one.
                        def ;
                        # Or that file.. it should never happen actually.
                        on_fail
                ]),8)
        ]),1)
 ]
