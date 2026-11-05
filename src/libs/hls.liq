let hls = {playlist = ()}

# The main playlist of an HLS output: one `EXT-X-STREAM-INF` per stream.
# @category Source / Output
# @param ~extra_tags Extra tags, each on its own line after the version.
# @param ~prefix Prefix of each media playlist URI.
# @param ~version Announced `EXT-X-VERSION`.
# @param streams Each media playlist's name, with its `bandwidth`, `codecs` (empty when unknown) and optional `video_size`.
def hls.playlist.main(
  ~extra_tags=[],
  ~prefix="",
  ~version=7,
  (
  streams:
  [string.{ bandwidth: int, codecs: string, video_size?: (int * int) }]
  )
) =
  def stream(s) =
    codecs = s.codecs == "" ? "" : ",CODECS=\"#{s.codecs}\""
    resolution =
      if
        null.defined(s?.video_size)
      then
        let (w, h) = null.get(s?.video_size)
        ",RESOLUTION=#{w}x#{h}"
      else
        ""
      end

    [
      "#EXT-X-STREAM-INF:BANDWIDTH=#{s.bandwidth}#{codecs}#{resolution}",
      output.file.hls.uri(prefix=prefix, "#{string(s)}.m3u8")
    ]
  end

  lines =
    [
      "#EXTM3U",
      "#EXT-X-VERSION:#{version}",
      ...extra_tags,
      ...list.flatten(list.map(stream, streams))
    ]

  string.concat(separator="\r\n", lines) ^ "\r\n"
end

# Output the source stream as an HTTP Live Streaming (HLS) presentation, with a
# main playlist written by `hls.playlist.main`.
# @category Source / Output
# @argsof output.file.hls
def replaces output.file.hls(
  %argsof(output.file.hls[!main_playlist_writer]),
  ~main_playlist_writer=null(
    fun (~extra_tags, ~prefix, ~version, streams) ->
      (
        hls.playlist.main(
          extra_tags=extra_tags,
          prefix=prefix,
          version=version,
          streams
        )
      :
        string?
      )
  ),
  dir,
  formats,
  s
) =
  output.file.hls(
    %argsof(output.file.hls[!main_playlist_writer]),
    main_playlist_writer=main_playlist_writer,
    dir,
    formats,
    s
  )
end

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

%ifdef harbor.http.register
# Output the source stream as HLS, served by the harbor HTTP server.
# @category Source / Output
# @argsof output.file.hls
# @param ~headers Headers of every response.
# @param ~port Port of the HTTP server.
# @param ~path Base path of the presentation.
# @param ~tmpdir Directory of the files; a new temporary directory when `null`.
# @param ~transport HTTP transport; use `http.transport.ssl` for HTTPS.
def output.harbor.hls(
  %argsof(output.file.hls),
  ~headers=[("Access-Control-Allow-Origin", "*")],
  ~port=8000,
  ~path="/",
  ~tmpdir=null,
  ~transport=http.transport.unix,
  formats,
  s
) =
  if
    null.defined(persist_at)
    and not null.defined(tmpdir)
    and not r/^\//.test(null.get(persist_at))
  then
    log.important(
      label="output.harbor.hls",
      "persist_at is relative and tmpdir is null: the state lands in a new \
       directory every run and can never be restored."
    )
  end

  dir =
    if
      null.defined(tmpdir)
    then
      null.get(tmpdir)
    else
      dir = file.temp_dir("hls")
      on_shutdown(fun () -> file.rmdir(dir))
      dir
    end

  o = output.file.hls(%argsof(output.file.hls), dir, formats, s)

  base = r/^\//.test(path) ? path : ("/#{path}")
  base = r/\/$/.test(base) ? base : ("#{base}/")

  def mime_of_extension(fname) =
    ext = ".#{file.extension(fname)}"
    list.find(
      default=("", ""),
      fun (m) -> snd(m) == ext,
      settings.http.mime.extnames()
    )
  end

  def respond(request, response, content_type, generated, fname) =
    names = list.map(fun (h) -> string.case(lower=true, fst(h)), headers)
    generated =
      list.filter(
        fun (h) -> not list.mem(string.case(lower=true, fst(h)), names),
        generated
      )

    content_type = content_type == "" ? [] : [("Content-Type", content_type)]

    if
      request.method == "GET"
    then
      data =
        try
          file.contents(fname)
        catch _ do
          null
        end
      if
        null.defined(data)
      then
        data = null.get(data)
        length = data == "" ? [("Content-Length", "0")] : []
        response.headers([...content_type, ...length, ...generated, ...headers])
        response.data(data)
      else
        response.status_code(404)
      end
    else
      length = file.size(fname)
      response.headers(
        [
          ...content_type,
          ("Content-Length", "#{length}"),
          ...generated,
          ...headers
        ]
      )
    end
  end

  def handler(request, response) =
    response.headers(headers)
    rpath = string.residual(prefix=base, request.path)
    if
      not null.defined(rpath)
    then
      response.status_code(404)
    else
      rel = url.decode(null.get(rpath))
      fname = "#{dir}/#{rel}"
      r = output.file.hls.lookup(o.id(), rel)
      if
        r.kind == "tracked" and file.exists(fname)
      then
        respond(request, response, r.content_type, r.headers, fname)
      elsif
        r.kind == "other"
        and file.exists(fname)
        and not file.is_directory(fname)
      then
        let (mime, _) = mime_of_extension(fname)
        respond(request, response, mime, [], fname)
      else
        response.status_code(404)
      end
    end
  end

  def register(method) =
    harbor.http.register(
      transport=transport,
      port=port,
      method=method,
      "#{base}.*",
      handler
    )
  end

  list.iter(register, ["GET", "HEAD", "OPTIONS"])
  o
end
%endif
