# Extract cover from the source's metadata and add it as a static image.
# @category Track / Video processing
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~default Default cover file when no cover is available
# @param ~mime_types Recognized mime types and their corresponding file extensions.
def video.add_cover(
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  ~mime_types=[
    ("image/gif", "gif"),
    ("image/jpg", "jpeg"),
    ("image/jpeg", "jpeg"),
    ("image/png", "png"),
    ("image/webp", "webp")
  ],
  ~default,
  s
) =
  cover_file = file.cover.manager(mime_types=mime_types, default=default)
  s = source.on_metadata(s, cover_file.set)
  video.add_request(
    fallible=fallible,
    x=x,
    y=y,
    width=width,
    height=height,
    request=cover_file,
    s
  )
end
