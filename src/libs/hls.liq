let input.hls = ()

# Play an HLS stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~reload How often (in seconds) the playlist should be reloaded.
# @param uri Playlist URI.
# @flag experimental
def input.hls.native(~id=null(),~reload=10.,uri)
  playlistr = ref([])
  sequence = ref(0)
  playlist_uri = ref(uri)
  id = string.id.default(default="input.hls.native", id)

  def load_playlist () =
    pl = request.create(playlist_uri())
    if request.resolve(pl) then
      pl = request.filename(pl)

      m = r/#EXT-X-MEDIA-SEQUENCE:(\d+)/.exec(file.contents(pl))
      pl_sequence = m[1]
      log.info(label=id,"Sequence: " ^ pl_sequence)
      pl_sequence = int_of_string(default=0,pl_sequence)

      files = playlist.parse(path=path.dirname(playlist_uri())^"/",pl)

      def file_request(idx,el) =
        let (meta,file) = el
        s = list.fold(fun (cur, el) -> "#{cur},#{fst(el)}=#{string.escape(encoding=\"ascii\", snd(el))}", "", meta)
        s = if s == "" then file else "annotate:#{s}:#{file}" end
        (pl_sequence+idx, s)
      end

      files = list.mapi(file_request,files)

      let (first_idx, _) = list.hd(default=(-1,""),playlistr())

      def add_file(playlist, file) =
        let (idx,_) = file
        if first_idx < idx and not list.assoc.mem(idx, playlist) then
          list.append(playlist, [file])
        else
          playlist
        end
      end

      playlistr := list.fold(add_file, playlistr(), files)
    else
      log.severe(label=id,"Couldn't read playlist: request resolution failed.")
      playlistr := []
    end
    request.destroy(pl)
  end

  def rec next () =
    if list.length(playlistr()) > 0 then
      let (_,ret) = list.hd(default=(1,""),playlistr())
      playlistr := list.tl(playlistr())
      sequence := sequence() + 1
      request.create(ret)
    else
      null()
    end
  end

  def find_stream () =
    pl = request.create(playlist_uri())
    if request.resolve(pl) then
      plfile = request.filename(pl)

      m = r/#EXT-X-STREAM-INF[^\n]*\n([^\r\n]*)\r?\n/.exec(file.contents(plfile))
      playlist_uri := list.assoc(default=playlist_uri(),1,m)

      if not (string.contains(substring="/", playlist_uri())) then
        playlist_uri := path.dirname(request.uri(pl)) ^ "/" ^ playlist_uri()
      end
      log(label=id,"Playlist: " ^ playlist_uri())
    end
  end
  find_stream ()

  s = request.dynamic(id=id,prefetch=10,next)

  let {track_marks=_, ...tracks} = source.tracks(s)
  s = source(tracks)

  thread.run(every=reload, load_playlist)

  s
end

let replaces input.hls = input.hls.native
%ifdef input.ffmpeg
let replaces input.hls = input.ffmpeg
%endif

# Play an HLS stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param uri Playlist URI.
def input.hls(~id=null(), uri)
  input.hls(id=id, uri)
end

let output.harbor.hls = ()

# @flag hidden
def output.harbor.hls.base(
  %argsof(output.file.hls[!segment_name]),
  ~segment_name,~tmpdir,~port,~path,serve,formats,s) =
  tmpdir = tmpdir ?? file.temp_dir("hls","")

  def content_type(fname) =
    ext = file.extension(fname)
    if ext == ".m3u8" then
      "application/x-mpegURL"
    else
      def f(cur,el) =
        format = snd(el)
        if ext == ".#{encoder.extension(format)}" then
          encoder.content_type(format)
        else
          cur
        end
      end
      list.fold(f,"",formats)
    end
  end

  serve(port=port,path=path,content_type=content_type,tmpdir)

  output.file.hls(
    %argsof(output.file.hls),
    tmpdir,formats,s)
end

# Output the source stream to an HTTP live stream served from the harbor HTTP server.
# @category Source / Output
# @argsof output.file.hls
# @param ~headers Default response headers.
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path for hls URIs.
# @param ~transport Http transport. Use `http.transport.ssl` or `http.transport.secure_transport`, when available, to enable HTTPS output
# @param ~tmpdir Directory for generated files.
# @param formats List of specifications for each stream: (name, format).
def replaces output.harbor.hls(
  %argsof(output.file.hls[!segment_name]),
  ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
  ~headers=[("Access-Control-Allow-Origin","*")],
  ~port=8000,~path="/",~tmpdir=null(),
  ~transport=http.transport.unix,
  formats,s) =
  def serve(~port,~path,~content_type,dir) =
    harbor.http.static(port=port,path=path,content_type=content_type,headers=headers,transport=transport,dir)
  end
  output.harbor.hls.base(%argsof(output.file.hls),path=path,port=port,tmpdir=tmpdir,serve,formats,s)
end

%ifdef harbor.https.static
# Output the source stream to an HTTP live stream served from the harbor HTTPS server.
# @category Source / Output
# @param ~headers Default response headers.
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path for hls URIs.
# @param ~tmpdir Directory for generated files.
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls.https(
  %argsof(output.file.hls[!segment_name]),
  ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
  ~headers=[("Access-Control-Allow-Origin","*")],
  ~port=8000,~path="/",~tmpdir=null(),
  formats,s) =
  def serve(~port,~path,~content_type,dir) =
    harbor.https.static(port=port,path=path,content_type=content_type,headers=headers,dir)
  end
  output.harbor.hls.base(%argsof(output.file.hls),path=path,port=port,tmpdir=tmpdir,serve,formats,s)
end
%endif
