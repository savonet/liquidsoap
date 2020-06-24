# Play an HLS stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~reload How often (in seconds) the playlist should be reloaded.
# @param uri Playlist URI
# @flag experimental
def input.hls(~id="",~reload=10.,uri)
  playlistr = ref([])
  sequence = ref(0)
  playlist_uri = ref(uri)

  def load_playlist () =
    pl = request.create(!playlist_uri)
    if request.resolve(pl) then
      pl = request.filename(pl)

      m = string.extract(pattern="#EXT-X-MEDIA-SEQUENCE:(\\d+)",file.contents(pl))
      pl_sequence = m[1]
      log.info(label=id,"Sequence: " ^ pl_sequence)
      pl_sequence = int_of_string(default=0,pl_sequence)

      files = playlist.parse(path=path.dirname(!playlist_uri)^"/",pl)

      def file_request(idx,el) =
        let (meta,file) = el
        s = list.fold(fun (cur, el) -> "#{cur},#{fst(el)}=#{string.escape(snd(el))}", "", meta)
        s = if s == "" then file else "annotate:#{s}:#{file}" end
        (pl_sequence+idx, s)
      end

      files = list.mapi(file_request,files)

      let (first_idx, _) = list.hd(default=(-1,""),!playlistr)

      def add_file(playlist, file) =
        let (idx,_) = file
        if first_idx < idx and not list.mem_assoc(idx, playlist) then
          list.append(playlist, [file])
        else
          playlist
        end
      end

      playlistr := list.fold(add_file, !playlistr, files)
    else
      log.severe(label=id,"Couldn't read playlist: request resolution failed.")
      playlistr := []
    end
    request.destroy(pl)
  end

  def rec next () =
    if list.length(!playlistr) > 0 then
      let (_,ret) = list.hd(default=(1,""),!playlistr)
      playlistr := list.tl(!playlistr)
      sequence := !sequence + 1
      [request.create(ret)]
    else
      []
    end
  end

  def find_stream () =
    pl = request.create(!playlist_uri)
    if request.resolve(pl) then
      plfile = request.filename(pl)

      m = string.extract(pattern="#EXT-X-STREAM-INF[^\\n]*\\n([^\\r\\n]*)\\r?\\n",file.contents(plfile))
      playlist_uri := list.assoc(default=!playlist_uri,1,m)

      if not (string.contains(substring="/", !playlist_uri)) then
        playlist_uri := path.dirname(request.uri(pl)) ^ "/" ^ !playlist_uri
      end
      log(label=id,"Playlist: " ^ !playlist_uri)
    end
  end
  find_stream ()

  source = request.dynamic.list(id=id,default_duration=0.5,length=5.,next)

  source = merge_tracks(source)

  thread.run(every=reload, load_playlist)

  source
end

let output.harbor.hls = ()

# @flag hidden
def output.harbor.hls.base(~id,~fallible,~on_start,~on_stop,~streams_info,
                           ~playlist,~segment_name,~segment_duration,
                           ~segments,~segments_overhead,~encode_metadata,
                           ~persist,~persist_at,~on_file_change,~start,
                           ~tmpdir,~port,~path,serve,formats,s)
  tmpdir = 
    if tmpdir != "" then
      tmpdir
    else
      file.temp_dir("hls","")
    end

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

  output.file.hls(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,streams_info=streams_info,
                  playlist=playlist,start=start,segment_name=segment_name,encode_metadata=encode_metadata,
                  segment_duration=segment_duration,segments=segments,on_file_change=on_file_change,
                  persist=persist,persist_at=persist_at,segments_overhead=segments_overhead,tmpdir,formats,s)
end

# Output the source stream to an HTTP live stream served from the harbor HTTP server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_file_change Callback executed when a file changes. `state` is one of: `"opened"`, `"closed"` or `"deleted"`, second argument is file path. Typical use: upload files to a CDN when done writting (`"close"` state and remove when `"deleted"`.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~encode_metadata Insert metadata into encoded stream. Note: Some HLS players (in particular android native HLS player) expect a single mpegts stream. Encoding metadata will break that assumption.
# @param ~persist Persist output accross restart. If enabled, generated files (segments and playlists) are kept on shutdown and the output state at the location given by `persist_at` and used on restart.
# @param ~persist_at Location of the configuration file used to restart the output when `persist=true`. Relative paths are assumed to be with regard to the directory for generated file.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segment_name Segment name. Default: `fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"`
# @param ~segments Number of segments per playlist.
# @param ~segments_overhead Number of segments to keep after they have been featured in the live playlist.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~streams_info Additional information about the streams. Should be a list of the form: `[(stream_name, (bandwidth, codec, extname)]`. See RFC 6381 for info about codec. Stream info are required when they cannot be inferred from the encoder.
# @param ~port Port for incoming harbor (http) connections.
# @param ~headers Default response headers.
# @param ~path Base path for hls URIs.
# @param ~tmpdir Directory for generated files.
# @param formats List of specifications for each stream: (name, format).
def replaces output.harbor.hls(~id="",~fallible=false,~on_start={()},~on_stop={()},~encode_metadata=false,
                      ~segment_duration=10.,~segments=15,~segments_overhead=10,~streams_info=[],~tmpdir="",
                      ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
                      ~start=true,~playlist="stream.m3u8",~port=8000,~path="/",~on_file_change=(fun (~state=_,_) -> ()),
                      ~headers=[("Access-Control-Allow-Origin","*")],~persist=false,~persist_at="state.config",formats,s)
  def serve(~port,~path,~content_type,dir) =
    harbor.http.static(port=port,path=path,content_type=content_type,headers=headers,dir)
  end
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,encode_metadata=encode_metadata,
                         segment_duration=segment_duration,segments=segments,on_file_change=on_file_change,
                         segments_overhead=segments_overhead,segment_name=segment_name,tmpdir=tmpdir,
                         start=start,port=port,path=path,playlist=playlist,streams_info=streams_info,
                         persist=persist,persist_at=persist_at,serve,formats,s)
end

%ifdef harbor.https.static
# Output the source stream to an HTTP live stream served from the harbor HTTPS server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_file_change Callback executed when a file changes. `state` is one of: `"opened"`, `"closed"` or `"deleted"`, second argument is file path. Typical use: upload files to a CDN when done writting (`"close"` state and remove when `"deleted"`.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~encode_metadata Insert metadata into encoded stream. Note: Some HLS players (in particular android native HLS player) expect a single mpegts stream. Encoding metadata will break that assumption.
# @param ~persist Persist output accross restart. If enabled, generated files (segments and playlists) are kept on shutdown and the output state at the location given by `persist_at` and used on restart.
# @param ~persist_at Location of the configuration file used to restart the output when `persist=true`. Relative paths are assumed to be with regard to the directory for generated file.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segment_name Segment name. Default: `fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"`
# @param ~segments Number of segments per playlist.
# @param ~segments_overhead Number of segments to keep after they have been featured in the live playlist.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~streams_info Additional information about the streams. Should be a list of the form: `[(stream_name, (bandwidth, codec, extname)]`. See RFC 6381 for info about codec. Stream info are required when they cannot be inferred from the encoder.
# @param ~port Port for incoming harbor (https) connections.
# @param ~headers Default response headers.
# @param ~path Base path for hls URIs.
# @param ~tmpdir Directory for generated files
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls.https(~id="",~fallible=false,~on_start={()},~on_stop={()},~encode_metadata=false,
                            ~segment_duration=10.,~segments=15,~segments_overhead=10,streams_info=[],
                            ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
                            ~port=8000,~path="/",~headers=[("Access-Control-Allow-Origin","*")],~tmpdir="",
                            ~on_file_change=(fun (~state=_,_) -> ()),~start=true,~persist=false,~persist_at="state.config",
                            ~playlist="stream.m3u8",formats,s)
  def serve(~port,~path,~content_type,dir) =
    harbor.https.static(port=port,path=path,content_type=content_type,headers=headers,dir)
  end
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                         segment_duration=segment_duration,segments=segments,encode_metadata=encode_metadata,
                         segments_overhead=segments_overhead,segment_name=segment_name,tmpdir=tmpdir,
                         start=start,port=port,path=path,playlist=playlist,streams_info=streams_info,
                         on_file_change=on_file_change,persist=persist,persist_at=persist_at,serve,formats,s)
end
%endif
