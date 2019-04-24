# Play an HLS stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param uri Playlist URI
def input.hls(~id="",uri)
  # A reference to the playlist
  playlist = ref []
  # Sequence number of the first element of the playlist
  sequence = ref 0
  # A reference to the uri
  playlist_uri = ref uri
  # A reference to know if the source has been stopped
  has_stopped = ref false
  # The load function
  def load_playlist () =
    pl = request.create.raw(!playlist_uri)
    if request.resolve(pl) then
      pl = request.filename(pl)
      # Find sequence number
      m = string.extract(pattern="#EXT-X-MEDIA-SEQUENCE:(\d+)",file.contents(pl))
      pl_sequence = list.assoc(default="","1",m)
      log(label=id,"Sequence: "^pl_sequence)
      pl_sequence = int_of_string(default=0,pl_sequence)
      # Fill the files
      files = playlist.parse(path=dirname(!playlist_uri)^"/",pl)
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
      log(label=id,"Couldn't read playlist: request resolution failed.")
      playlist := []
    end
    request.destroy(pl)
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
        if not !has_stopped then
          has_stopped := true
        end
        ""
      end
    # Look for new files
    load_playlist()
    request.create(file)
  end
  # Instanciate the source
  source = request.dynamic(id=id,next)
  # Populate the playlist
  load_playlist()
  # Return
  source
end
