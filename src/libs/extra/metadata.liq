# Store and retrieve file covers using metadata. This returns a set of
# getter/setter methods that can be used to store and retrieve cover art.
# Typical usage is to set cover art in a `on_metadata` handler and retrieve
# it in a `video.add_image` operator. See `video.add_cover` for an implementation
# example.
# @category Metadata
# @flag extra
# @param ~default Default cover file when no cover is available
# @param ~mime_types Recognized mime types and their corresponding file extensions.
def file.cover.manager(
  ~id=null.make(),
  ~mime_types=[
    ("image/gif", "gif"),
    ("image/jpg", "jpeg"),
    ("image/jpeg", "jpeg"),
    ("image/png", "png"),
    ("image/webp", "webp")
  ],
  ~default
) =
  id = string.id.default(id, default="cover")
  default = request.create(persistent=true, default)

  current_cover_request = ref(default)

  def extract_cover_from_metadata(_metadata) =
    filename = _metadata["filename"]
    log.info(
      label=id,
      "Extracting cover from #{string.quote(filename)}."
    )
    cover = metadata.cover(_metadata)

    new_cover =
      if
        not null.defined(cover)
      then
        log.important(
          label=id,
          "File #{string.quote(filename)} has no cover."
        )
        null.make()
      else
        cover = null.get(cover)
        extension = mime_types[cover.mime]
        if
          extension == ""
        then
          log.important(
            label=id,
            "File #{string.quote(filename)} has unknown mime type #{
              string.quote(cover.mime)
            }."
          )
          null.make()
        else
          cover_file = file.temp("#{id}_", ".#{extension}")
          file.write(cover_file, data=cover)

          log.important(
            label=id,
            "Cover for #{string.quote(filename)} saved to #{
              string.quote(cover_file)
            }."
          )

          request.create(temporary=true, cover_file)
        end
      end

    current_cover_request := (new_cover ?? default)
  end

  current_cover_request.{set=extract_cover_from_metadata}
end
