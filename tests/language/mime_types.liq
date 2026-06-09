def f() =
  # Basic lookups
  test.equal(file.mime.extension("audio/mpeg"), ".mp3")
  test.equal(file.mime.extension("image/png"), ".png")
  test.equal(file.mime.extension("image/jpeg"), ".jpg")
  test.equal(file.mime.extension("audio/ogg"), ".ogg")

  # Case-insensitive
  test.equal(file.mime.extension("Audio/MPEG"), ".mp3")
  test.equal(file.mime.extension("IMAGE/PNG"), ".png")
  test.equal(file.mime.extension("Image/Jpeg"), ".jpg")

  # Unknown mime type returns null
  test.equal(file.mime.extension("unknown/type"), null)

  # Custom mime_types_extnames overrides defaults
  test.equal(
    file.mime.extension(
      mime_types_extnames=[("image/png", ".PNG")], "image/png"
    ),
    ".PNG"
  )

  # Custom mime_types_extnames: unknown type returns null
  test.equal(
    file.mime.extension(
      mime_types_extnames=[("image/png", ".png")], "audio/mpeg"
    ),
    null
  )

  test.pass()
end

test.check(f)
