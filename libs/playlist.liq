# Custom playlist source written using the script language.  Will read directory
# or playlist, play all files and stop.  Returns a pair `(reload,source)` where
# `reload` is a function of type `(?uri:string)->unit` used to reload the source
# and `source` is the actual source. The reload function can optionally be
# called with a new playlist URI. Otherwise, it reloads the previous URI.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~random Randomize playlist content
# @param ~on_done Function to execute when the playlist is finished
# @param ~filter Filter out some files depending on metadata
# @param uri Playlist URI
def playlist.reloadable(~id="",~random=false,~on_done={()},
                        ~default_duration=30.,~length=10.,~conservative=false,
                        ~filter=fun(_)->true,uri)
  # A reference to the playlist
  playlist = ref []
  # A reference to the uri
  playlist_uri = ref uri
  # A reference to know if the source has been stopped
  has_stopped = ref false
  # The next function
  def rec next () =
    file =
      if list.length(!playlist) > 0 then
        ret = list.hd(default="",!playlist)
        playlist := list.tl(!playlist)
        ret
      else
        # Playlist finished
        if not !has_stopped then
          has_stopped := true
          on_done ()
        end
        ""
      end
    r = request.create(file)
    if filter(request.metadata(r)) then
      r
    else
      next ()
    end
  end
  # Instanciate the source
  source = request.dynamic(id=id,conservative=conservative,default_duration=default_duration,
                           length=length,next)
  # Get its id.
  id = source.id(source)
  # The load function
  def load_playlist () =
    files =
      if file.is_directory(!playlist_uri) then
        log(label=id,"Playlist is a directory.")
        files = file.ls(absolute=true,recursive=true,!playlist_uri)
        list.filter(fun(f) -> not (file.is_directory(f)), files)
      else
        playlist = request.create.raw(!playlist_uri)
        result =
          if request.resolve(playlist) then
            playlist = request.filename(playlist)
            files = playlist.parse(playlist)
            def file_request(el) =
              let (meta, file) = el
              s = list.fold(fun (cur, el) ->
                "#{cur},#{fst(el)}=#{string.escape(snd(el))}", "", meta)
              if s == "" then
                file
              else
                "annotate:#{s}:#{file}"
              end
            end
            list.map(file_request,files)
          else
            log(label=id,"Couldn't read playlist: request resolution failed.")
            []
          end
        request.destroy(playlist)
        result
      end
    if random then
      playlist := list.sort(fun (x,y) -> int_of_float(random.float()), files)
    else
      playlist := files
    end
  end
  # The reload function
  def reload(~uri="") =
    if uri != "" then
      playlist_uri := uri
    end
    log(label=id,"Reloading playlist with URI #{!playlist_uri}")
    has_stopped := false
    load_playlist()
  end
  # Load the playlist
  load_playlist()
  # Return
  (reload,source)
end

# Custom playlist source written using the script language. It will read directory
# or playlist, play all files and stop.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~random Randomize playlist content
# @param ~on_done Function to execute when the playlist is finished
# @param ~reload_mode If set to "watch", will be reloaded when the playlist is changed
# @param ~filter Filter out some files depending on metadata
# @param uri Playlist URI
def playlist.once(~id="",~random=false,~on_done={()},~reload_mode="",
                  ~default_duration=30.,~length=10.,~conservative=false,
                  ~filter=fun(_)->true,uri)
  let (reload, s) = playlist.reloadable(id=id,random=random,on_done=on_done,
                           conservative=conservative,default_duration=default_duration,
                           filter=filter,uri)
  if reload_mode == "watch" then
    unwatch = file.watch(uri,fun () -> reload())
    source.on_shutdown(s,unwatch)
  end
  s
end

# Play the whole playlist as one track.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~random Randomize playlist content
# @param uri Playlist URI.
def playlist.merge(~id="",~random=false,
                   ~default_duration=30.,~length=10.,~conservative=false,uri) =
  pl = playlist.reloadable(id=id,random=random,
                           conservative=conservative,default_duration=default_duration,
                           uri)
  let (reload, s) = pl
  s = merge_tracks(s)
  on_end(delay=0.,fun(_,_)->reload(),s)
end

# Play a queue of uris. Returns a function to push new uris in the queue as well as the resulting source.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~queue Initial queue of requests.
# @param Timeout (in sec.) for a single download.
def request.queue.pushable(~id="", ~conservative=false, ~default_duration=30., ~interactive=true, ~length=40., ~queue=[], ~timeout=20.)
  queue = ref(queue)
  def next()
    uri = list.hd(default="", !queue)
    queue := list.tl(!queue)
    log.info(label=id, "Next song will be #{uri}.")
    request.create(uri)
  end
  def push(uri)
    log.info(label=id, "Pushing #{uri} on the queue.")
    queue := list.append(!queue, [uri])
  end
  s = request.dynamic(id=id, conservative=conservative, default_duration=default_duration, length=length, timeout=timeout, available={!queue != []}, next)
  if interactive then
    def push(uri) = push(uri); "done" end
    server.register(namespace="#{id}", description="Push a new request in the queue.", usage="push <uri>", "push", push)
    def show_queue(_) = string.concat(separator="\n", !queue) end
    server.register(namespace="#{id}", description="Display current queue content.", usage="queue", "queue", show_queue)
    def skip(_) = source.skip(s); "done" end
    server.register(namespace="#{id}", description="Skip current song.", usage="skip", "skip", skip)
  end
  (push, s)
end

# Play a queue of uris. This function is mainly meant to be controlled over telnet.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~queue Initial queue of requests.
# @param Timeout (in sec.) for a single download.
def request.queue(~id="", ~conservative=false, ~default_duration=30., ~interactive=true, ~length=40., ~queue=[], ~timeout=20.)
  snd(request.queue.pushable(id=id, conservative=conservative, default_duration=default_duration, interactive=interactive, length=length, queue=queue, timeout=timeout))
end
