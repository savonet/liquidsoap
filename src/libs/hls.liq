let hls = {playlist = ()}

let input.hls = ()

# Play an HLS stream.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param ~reload How often (in seconds) the playlist should be reloaded.
# @param uri Playlist URI.
# @flag experimental
def input.hls.native(~id=null, ~reload=10., uri) =
  playlistr = ref([])
  sequence = ref(0)
  playlist_uri = ref(uri)
  id = string.id.default(default="input.hls.native", id)

  def load_playlist() =
    pl = request.create(playlist_uri())
    if
      request.resolve(pl)
    then
      pl = request.filename(pl)
      m = r/#EXT-X-MEDIA-SEQUENCE:(\d+)/.exec(file.contents(pl))
      pl_sequence = m[1]
      log.info(
        label=id,
        "Sequence: " ^
          pl_sequence
      )
      pl_sequence = int_of_string(default=0, pl_sequence)
      files = playlist.parse(path=path.dirname(playlist_uri()) ^ "/", pl)

      def file_request(idx, el) =
        let (meta, file) = el

        def escape(s) =
          string.escape(encoding="ascii", s)
        end

        s =
          list.fold(
            fun (cur, el) -> "#{cur},#{fst(el)}=#{escape(snd(el))}",
            "",
            meta
          )

        s = if s == "" then file else "annotate:#{s}:#{file}" end
        (pl_sequence + idx, s)
      end

      files = list.mapi(file_request, files)
      let (first_idx, _) = list.hd(default=(-1, ""), playlistr())

      def add_file(playlist, file) =
        let (idx, _) = file
        if
          first_idx < idx and not list.assoc.mem(idx, playlist)
        then
          list.append(playlist, [file])
        else
          playlist
        end
      end

      playlistr := list.fold(add_file, playlistr(), files)
    else
      log.severe(
        label=id,
        "Couldn't read playlist: request resolution failed."
      )
      playlistr := []
    end

    request.destroy(pl)
  end

  def rec next() =
    if
      list.length(playlistr()) > 0
    then
      let (_, ret) = list.hd(default=(1, ""), playlistr())
      playlistr := list.tl(playlistr())
      sequence := sequence() + 1
      request.create(ret)
    else
      null
    end
  end

  def find_stream() =
    pl = request.create(playlist_uri())
    if
      request.resolve(pl)
    then
      plfile = request.filename(pl)
      m =
        r/#EXT-X-STREAM-INF[^\n]*\n([^\r\n]*)\r?\n/.exec(file.contents(plfile))

      playlist_uri := list.assoc(default=playlist_uri(), 1, m)
      if
        not (string.contains(substring="/", playlist_uri()))
      then
        playlist_uri := path.dirname(request.uri(pl)) ^ "/" ^ playlist_uri()
      end

      log(
        label=id,
        "Playlist: " ^
          playlist_uri()
      )
    end
  end

  find_stream()
  s = request.dynamic(id=id, prefetch=10, next)
  let {track_marks = _, ...tracks} = source.tracks(s)
  s = source(tracks)
  thread.run(every=reload, load_playlist)
  s
end

let replaces input.hls = input.hls.native

%ifdef input.ffmpeg
let replaces input.hls = fun (~id=null, uri) -> input.ffmpeg(id=id, uri)
%endif

# Play an HLS stream.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param uri Playlist URI.
def input.hls(~id=null, uri) =
  input.hls(id=id, uri)
end
