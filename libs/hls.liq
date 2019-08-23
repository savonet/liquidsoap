# Play an HLS stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~reload How often (in seconds) the playlist should be reloaded.
# @param uri Playlist URI
def input.hls(~id="",~reload=10.,uri)
  # A reference to the playlist
  playlist = ref []
  # Sequence number of the first element of the playlist
  sequence = ref 0
  # A reference to the uri
  playlist_uri = ref uri
  # The load function
  def load_playlist () =
    pl = request.create.raw(!playlist_uri)
    if request.resolve(pl) then
      pl = request.filename(pl)
      # Find sequence number
      m = string.extract(pattern="#EXT-X-MEDIA-SEQUENCE:(\\d+)",file.contents(pl))
      pl_sequence = list.assoc(default="","1",m)
      log.info(label=id,"Sequence: " ^ pl_sequence)
      pl_sequence = int_of_string(default=0,pl_sequence)
      # Fill the files
      files = playlist.parse(path=path.dirname(!playlist_uri)^"/",pl)
      def file_request(el) =
        let (meta,file) = el
        s = list.fold(fun (cur, el) -> "#{cur},#{fst(el)}=#{string.escape(snd(el))}", "", meta)
        if s == "" then file else "annotate:#{s}:#{file}" end
      end
      if !playlist == [] then
        playlist := list.map(file_request,files)
        sequence := pl_sequence
      else
        # Add only the files after the sequence
        def rec aux(s,files) =
          if files != [] then
            if s == !sequence + list.length(!playlist) then
              files = list.map(file_request,files)
              playlist := list.append(!playlist,files)
            else
              aux(s+1,list.tl(files))
            end
          end
        end
        aux(pl_sequence, files)
      end
    else
      log.severe(label=id,"Couldn't read playlist: request resolution failed.")
      playlist := []
    end
    request.destroy(pl)
    reload
  end
  # The next function
  def rec next () =
    file =
      if list.length(!playlist) > 0 then
        ret = list.hd(default="",!playlist)
        playlist := list.tl(!playlist)
        sequence := !sequence + 1
        ret
      else
        # Playlist finished
        ""
      end
    request.create(file)
  end
  # Find the stream in case there are many
  def find_stream () =
    pl = request.create.raw(!playlist_uri)
    if request.resolve(pl) then
      plfile = request.filename(pl)
      # Find sequence number
      m = string.extract(pattern="#EXT-X-STREAM-INF[^\n]*\n([^\r\n]*)\r?\n",file.contents(plfile))
      playlist_uri := list.assoc(default=!playlist_uri,"1",m)
      # Add prefix if necessary
      if not (string.contains(substring="/", !playlist_uri)) then
        playlist_uri := path.dirname(request.uri(pl)) ^ "/" ^ !playlist_uri
      end
      log(label=id,"Playlist: " ^ !playlist_uri)
    end
  end
  find_stream ()
  # Instanciate the source
  source = request.dynamic(id=id,next)
  # Merge the tracks
  source = merge_tracks(source)
  # Populate the playlist
  add_timeout(0., load_playlist)
  # Return
  source
end

# @flag hidden
def output.harbor.hls.base(~id,~fallible,~on_start,~on_stop,
                           ~playlist,~segment_name,~segment_duration,
                           ~segments,~segments_per_playlist,
                           ~start,~port,~path,serve,formats,s)
  tmp_dir = file.temp_dir("hls","")

  serve(port=port,path=path,tmp_dir)

  output.file.hls(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                  playlist=playlist,start=start,segment_name=segment_name,
                  segment_duration=segment_duration,segments=segments,
                  segments_per_playlist=segments_per_playlist,tmp_dir,formats,s)
end

# Output the source stream to an HTTP live stream served from the harbor HTTP server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segment_name Segment name. Default: `fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"`
# @param ~segments Number of segments to keep.
# @param ~segments_per_playlist Number of segments per playlist.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~port Port for incoming harbor (http) connections.
# @param ~headers Default response headers.
# @param ~path Base path for hls URIs.
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls(~id="",~fallible=false,~on_start={()},~on_stop={()},
                      ~segment_duration=10.,~segments=15,~segments_per_playlist=10,
                      ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
                      ~start=true,~playlist="stream.m3u8",~port=8000,~path="/",
                      ~headers=[("Access-Control-Allow-Origin","*")],formats,s)
  def serve(~port,~path,dir) =
    harbor.http.static(port=port,path=path,headers=headers,dir)
  end
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                         segment_duration=segment_duration,segments=segments,
                         segments_per_playlist=segments_per_playlist,segment_name=segment_name,
                         start=start,port=port,path=path,playlist=playlist,
                         serve,formats,s)
end

%ifdef harbor.https.static
# Output the source stream to an HTTP live stream served from the harbor HTTPS server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segment_name Segment name. Default: `fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"`
# @param ~segments Number of segments to keep.
# @param ~segments_per_playlist Number of segments per playlist.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~port Port for incoming harbor (https) connections.
# @param ~headers Default response headers.
# @param ~path Base path for hls URIs.
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls.https(~id="",~fallible=false,~on_start={()},~on_stop={()},
                            ~segment_duration=10.,~segments=15,~segments_per_playlist=10,
                            ~segment_name=(fun (~position,~extname,stream_name) -> "#{stream_name}_#{position}.#{extname}"),
                            ~port=8000,~path="/",~headers=[("Access-Control-Allow-Origin","*")],
                            ~start=true,~playlist="stream.m3u8",formats,s)
  def serve(~port,~path,dir) =
    harbor.https.static(port=port,path=path,headers=headers,dir)
  end
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                         segment_duration=segment_duration,segments=segments,
                         segments_per_playlist=segments_per_playlist,segment_name=segment_name,
                         start=start,port=port,path=path,playlist=playlist,
                         serve,formats,s)
end
%endif
