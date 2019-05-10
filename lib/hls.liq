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
      log(label=id,level=4,"Sequence: " ^ pl_sequence)
      pl_sequence = int_of_string(default=0,pl_sequence)
      # Fill the files
      files = playlist.parse(path=path.dirname(!playlist_uri)^"/",pl)
      def file_request(el) =
        meta = fst(el)
        file = snd(el)
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
      log(label=id,level=1,"Couldn't read playlist: request resolution failed.")
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
      pl = request.filename(pl)
      # Find sequence number
      m = string.extract(pattern="#EXT-X-STREAM-INF[^\n]*\n([^\n]*)\n",file.contents(pl))
      playlist_uri := list.assoc(default=!playlist_uri,"1",m)
      log(label=id,"Playlist: " ^ !playlist_uri)
    end
  end
  find_stream ()
  # Instanciate the source
  source = request.dynamic(id=id,next)
  # Populate the playlist
  add_timeout(0., load_playlist)
  # Return
  source
end

# @flag hidden
def output.harbor.hls.base(~id,~fallible,~on_start,~on_stop,
                           ~playlist,~segment_duration,~segments,
                           ~start,~port,~path,serve,formats,s)
  tmp_dir = file.temp_dir("hls","")

  on_shutdown(fun () -> file.rmdir(tmp_dir))

  path =
    if string.sub(path,start=0,length=1) != "/" then
      "/#{path}"
    else
      path
    end

  def handler(~method,~protocol,~data,~headers,uri)
    ret = string.extract(pattern="^#{path}([^?]*)",uri)

    if list.length(ret) == 0 then
      http_response(protocol=protocol,code=404,data="Not Found!")
    else
      match = list.hd(default=("1","foo"),ret)
      file = path.concat(tmp_dir, snd(match))
      if file.exists(file) then
        if method == "OPTIONS" then
          http_response(protocol=protocol,code=200)
        else
          http_response(protocol=protocol,code=200,data=file.contents(file))
        end
      else
        http_response(protocol=protocol,code=404,data="Not Found!")
      end
    end      
  end

  def register(method)
    serve(port=port,method=method,"^#{path}",handler(method=method))
  end
  list.iter(register, ["OPTIONS","GET"])

  output.file.hls(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                  playlist=playlist,start=start,
                  segment_duration=segment_duration,segments=segments,
                  tmp_dir,formats,s)
end

# Output the source stream to an HTTP live stream served from the harbor HTTP server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segments Number of segments to keep.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~port Port for incoming harbor (http) connections.
# @param ~path Base path for hls URIs.
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls(~id="",~fallible=false,~on_start={()},~on_stop={()},
                      ~segment_duration=10.,~segments=10,~start=true,
                      ~playlist="stream.m3u8",~port=8000,~path="/",formats,s)
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                         segment_duration=segment_duration,segments=segments,
                         start=start,port=port,path=path,playlist=playlist,
                         harbor.http.register,formats,s)
end

%ifdef harbor.https.register
# Output the source stream to an HTTP live stream served from the harbor HTTPS server.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~playlist Playlist name (m3u8 extension is recommended).
# @param ~segment_duration Segment duration (in seconds).
# @param ~segments Number of segments to keep.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~port Port for incoming harbor (https) connections.
# @param ~path Base path for hls URIs.
# @param formats List of specifications for each stream: (name, format).
def output.harbor.hls.https(~id="",~fallible=false,~on_start={()},~on_stop={()}
                            ~segment_duration=10.,~segments=10,~start=true,
                            ~port=8000,~path="/",~playlist="stream.m3u8",formats,s)
  output.harbor.hls.base(id=id,fallible=fallible,on_start=on_start,on_stop=on_stop,
                         segment_duration=segment_duration,segments=segments,
                         start=start,port=port,path=path,playlist=playlist,
                         harbor.https.register,formats,s)
end
%endif
