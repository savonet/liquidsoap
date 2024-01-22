# Store and retrieve file covers using metadata. This returns a set of
# getter/setter methods that can be used to store and retrieve cover art.
# Typical usage is to set cover art in a `on_metadata` handler and retrieve
# it in a `video.add_image` operator.
# @category Metadata
# @param ~default Default cover file when no cover is available
# @param ~mime_types Recognized mime types and their corresponding file extensions.
def file.cover.manager(
  ~id=null(),
  ~mime_types=[
    ("image/gif", "gif"),
    ("image/jpeg", "jpeg"),
    ("image/png", "png"),
    ("image/webp", "webp")
  ],
  ~default
) =
  id = string.id.default(id, default="cover")
  temp_dir = file.temp_dir("cover-manager")
  on_cleanup({file.rmdir(temp_dir)})

  current_cover_file = ref(default)

  def set_current_cover(filename) =
    if
      current_cover_file() != default
    then
      log.info(
        label=id,
        "Removing #{string.quote(current_cover_file())}"
      )
      file.remove(current_cover_file())
    end
    current_cover_file := filename
  end

  def save_cover_to_file(cover) =
    mime = cover.mime
    extension = mime_types[mime]

    cover_file =
      if
        extension != ""
      then
        cover_file = file.temp(dir=temp_dir, "#{id}_", ".#{extension}")
        file.write(cover_file, data=cover)
        cover_file
      else
        null()
      end
    (cover_file, mime)
  end

  def extract_cover_from_metadata(_metadata) =
    filename = _metadata["filename"]
    log.info(
      label=id,
      "Extracting cover from #{string.quote(filename)}."
    )
    cover = metadata.cover(_metadata)
    let (cover_file, mime) =
      null.case(cover, {(null(), "")}, save_cover_to_file)

    if
      cover == null()
    then
      log.important(
        label=id,
        "File #{string.quote(filename)} has no cover."
      )
      set_current_cover(default)
    elsif
      cover_file == null()
    then
      log.important(
        label=id,
        "File #{string.quote(filename)} has unknown mime type #{
          string.quote(mime)
        }."
      )
      set_current_cover(default)
    else
      cover_file = null.get(cover_file)
      log.important(
        label=id,
        "Cover for #{string.quote(filename)} saved to #{
          string.quote(cover_file)
        }."
      )
      set_current_cover(cover_file)
    end
  end

  current_cover_file.{set=extract_cover_from_metadata}
end
