def filter_meta(meta) =
  [("title", meta["title"])]
end

def f() =
  fname = "file1.mp3"
  meta = file.metadata(fname)
  test.equal(
    filter_meta(meta),
    [
      (
        "title",
        "Test Title"
      )
    ]
  )

  r =
    request.create(
      "annotate:title=\"Annotate Override\":#{fname}"
    )
  test.equal(request.resolve(r), true)
  test.equal(
    filter_meta(request.metadata(r)),
    [
      (
        "title",
        "Annotate Override"
      )
    ]
  )

  decoder.metadata.add(
    priority=-1,
    "test-no-override",
    fun (~metadata:_, _) ->
      [
        (
          "title",
          "Metadata decoder no override"
        )
      ]
  )

  meta = file.metadata(fname)
  test.equal(
    filter_meta(meta),
    [
      (
        "title",
        "Test Title"
      )
    ]
  )

  r =
    request.create(
      "annotate:title=\"Annotate Override\":#{fname}"
    )
  test.equal(request.resolve(r), true)
  test.equal(
    filter_meta(request.metadata(r)),
    [
      (
        "title",
        "Annotate Override"
      )
    ]
  )

  decoder.metadata.add(
    priority=4,
    "test-override",
    fun (~metadata:_, _) ->
      [
        (
          "title",
          "Metadata decoder override"
        )
      ]
  )

  meta = file.metadata(fname)
  test.equal(
    filter_meta(meta),
    [
      (
        "title",
        "Metadata decoder override"
      )
    ]
  )

  r =
    request.create(
      "annotate:title=\"Annotate Override\":#{fname}"
    )
  test.equal(request.resolve(r), true)
  test.equal(
    filter_meta(request.metadata(r)),
    [
      (
        "title",
        "Annotate Override"
      )
    ]
  )

  decoder.metadata.add(
    priority=10,
    "test-override-annotate",
    fun (~metadata:_, _) ->
      [
        (
          "title",
          "Metadata decoder annotate override"
        )
      ]
  )

  r =
    request.create(
      "annotate:title=\"Annotate Override\":#{fname}"
    )
  test.equal(request.resolve(r), true)
  test.equal(
    filter_meta(request.metadata(r)),
    [
      (
        "title",
        "Metadata decoder annotate override"
      )
    ]
  )

  test.pass()
end

test.check(f)
