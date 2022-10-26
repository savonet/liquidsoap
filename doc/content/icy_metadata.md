ICY metadata
============
*ICY metadata* is the name for the mechanism used to update
metadata in icecast's source streams.
The techniques is primarily intended for data formats that do not support in-stream
metadata, such as mp3 or AAC. However, it appears that icecast also supports
ICY metadata update for ogg/vorbis streams.

When using the ICY metadata update mechanism, new metadata are submitted separately from
the stream's data, via a http GET request. The format of the request depends on the
protocol you are using (ICY for shoutcast and icecast 1 or HTTP for icecast 2).

Starting with 1.0, you can do several interesting things with icy metadata updates
in liquidsoap. We list some of those here.

Enable/disable ICY metadata updates
-----------------------------------
You can enable or disable icy metadata update in `output.icecast`
by setting the `icy_metadata` parameter to either `"true"`
or `"false"`. The default value is `"guess"` and does the following:

* Set `"true"` for: mp3, aac, aac+, wav
* Set `"false"` for any format using the ogg container

You may, for instance, enable icy metadata update for ogg/vorbis
streams.

Update metadata manually
------------------------
The function `icy.update_metadata` implements a manual metadata update
using the ICY mechanism. It can be used independently from the `icy_metadata`
parameter described above, provided icecast supports ICY metadata for the intended stream.

For instance the following script registers a telnet command name `metadata.update`
that can be used to manually update metadata:

```liquidsoap
def icy_update(v) =
  # Parse the argument
  l = string.split(separator=",",v)
  def split(l,v) =
    v = string.split(separator="=",v)
    if list.length(v) >= 2 then
      list.append(l,[(list.nth(v,0,default=""),list.nth(v,1,default=""))])
    else
      l
    end
  end
  meta = list.fold(split,[],l)

  # Update metadata
  icy.update_metadata(mount="/mystream",password="hackme",
                      host="myserver.net",meta)
  "Done !"
end

server.register("update",namespace="metadata",
                 description="Update metadata",
                 usage="update title=foo,album=bar,..",
                 icy_update)
```

As usual, `liquidsoap -h icy.update_metadata` lists all the arguments
of the function.
