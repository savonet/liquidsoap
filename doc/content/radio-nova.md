An advanced script to listen to radio nova.
===========================================
Introduction
------------
[Radio Nova](http://www.novaplanet.com/) is a french eclectic radio that provides a mp3 stream
via Icecast. However, they don't include the metadata (probably to avoid rippers..).

Still, these metadata are available on their webpage:
```
14:29 toots@leonard /tmp% wget -q http://www.novaplanet.com -O - | grep 'scroll_play'
   scroll_play("ESTHER PHILLIPS","DISPOSABLE SOCIETY", 'playing_now', '0');
```

Hence, it would be nice to dynamically grab these metadata and insert it in the stream. The following 
script will perform this.

The script
----------
```liquidsoap
#!/usr/bin/liquidsoap
# Liquidsoap script to listen to radio nova, grabbing metadata
# on the webpage.

# Disable file log, enable stdout log
set("log.file",false)
set("log.stdout",true)

# Initial input.http source
nova = input.http("http://broadcast.infomaniak.net:80/radionova-high.mp3")
# Remove metadata, add a hook to insert new metadata
nova = insert_metadata(id="nova",
         clear_metadata(nova))

# This string references will be used to keed track
# of previous metadata
title = ref("unknown title")
artist = ref("unknown artist")

# Capitalize and lowercase
def cap(s)
  string.capitalize(string.case(s))
end

# Process to grab metadata on the webpage
# Returns "artist","title"
def metas() = 
  s = list.hd(
        default="",
        get_process_lines(
	   "wget -q http://www.novaplanet.com -O - | grep 'scroll_play'"))
  s = string.extract(pattern='scroll_play\("([^"]*)",\s*"([^"]*)"',s)
  (cap(list.assoc(default="","1",s)),cap(list.assoc(default="","2",s)))
end

# Process that inserts grabbed metadata
# to the stream
def add_meta_nova()
  log = log(level=4)
  log("Checking for metas")
  x = metas()
  new_artist = fst(x)
  new_title = snd(x)
  old_title = !title
  old_artist = !artist
  if (old_artist != new_artist or old_title != new_title) and 
     (new_title != "" or new_artist != "") 
  then
    log("Got new metas: #{new_artist} -- #{new_title}")
    ignore(
      server.execute(
        'nova.insert artist="#{new_artist}",title="#{new_title}"'))
    title := new_title
    artist := new_artist
  else
    log("Keeping old metas")
  end
  0.2
end

# Schedule the insert process every 0.2 second
add_timeout(fast=false,0.2,add_meta_nova)

# function to display new metadatas
def print_meta(m) = 
  def print(z) = 
    label = fst(z)
    value = snd(z)
    log("Metadata: #{label}=#{value}")
  end
  list.iter(print,m)
end

# Hook the previous function on the stream
nova = on_metadata(print_meta,nova)

# Play the stream !
out(nova)
```


