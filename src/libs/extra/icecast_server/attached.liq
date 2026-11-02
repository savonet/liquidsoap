# Liquidsoap sources attached to icecast.server mounts (mounts.md §2): the
# server's `attach_source` and `detach_source` methods call
# `icecast.server.attached.attach` and `detach`, which go through the feeders
# of feeders.liq.
let icecast.server.attached = ()

# Feeds the mount at `path` from the Liquidsoap source `s` of encoded content,
# encoded for listeners by `format` (mounts.md §2).
# @flag hidden
def icecast.server.attached.attach(state, ~headers, ~format, path, s) =
  if
    icecast.server.sources.is_reserved(path)
  then
    error.raise(
      error.icecast.server.reserved_path,
      "#{path} is a reserved path"
    )
  end
  feeder =
    icecast.server.mounts.feeder(
      is_client=false,
      identity=icecast.server.sources.identity(headers),
      headers=[],
      ip="",
      agent="",
      format={
        container = null,
        codecs = [],
        mime = encoder.content_type(format),
        channels = null,
        samplerate = null
      },
      encoder=fun (_) -> format,
      stop=fun () -> (),
      bytes_read=fun () -> 0,
      s
    )
  if
    not icecast.server.feeders.attach(state, strict=true, path, feeder)
  then
    error.raise(
      error.icecast.server.mount_taken,
      "#{path} already has a feeder"
    )
  end
end

# Stops feeding `path` from its attached Liquidsoap source, if it has one.
# @flag hidden
def icecast.server.attached.detach(state, path) =
  entry = icecast.server.mounts.find(state, path)
  feeder = null.defined(entry) ? null.get(entry).feeder.get() : null
  if
    null.defined(feeder) and not null.get(feeder).is_client
  then
    icecast.server.feeders.detach(state, path, null.get(feeder))
  end
end
