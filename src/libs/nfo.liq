# Parse XML `.nfo` sidecar files (Kodi/Emby/Jellyfin style).
#
# This is meant to be used either directly via `file.nfo.metadata(...)` or as a
# metadata resolver through `enable_nfo_metadata(...)`.

# @category File
let file.nfo = ()

# Accumulate values by key.
def file.nfo._accum_add(k, v, acc) =
  if
    list.assoc.mem(k, acc)
  then
    vs = list.assoc(k, acc)
    (k, v::vs)::list.assoc.remove(k, acc)
  else
    (k, [v])::acc
  end
end

def file.nfo._join(vs) =
  string.concat(separator=";", list.rev(vs))
end

# Extract metadata from an `.nfo` file.
#
# Returned metadata keys are namespaced under `nfo.*`.
#
# Currently, only direct children of the root element that contain textual
# content are extracted.
#
# Special case:
# - `<uniqueid type="X">VALUE</uniqueid>` is exported as `nfo.uniqueid.X=VALUE`.
#
# @category File
# @param nfo_file Path to the `.nfo` file.
def file.nfo.metadata(nfo_file) =
  if
    not (file.exists(nfo_file))
  then
    []
  else
    s = file.contents(nfo_file)
    try
      let xml.parse (doc :
        (
          string
          *
          {
            xml_params: [(string * string)],
            xml_children:
              [
                (
                  string
                  *
                  {
                    xml_params: [(string * string)],
                    xml_children: [(string * {xml_text: string?})]
                  }
                )
              ]
          }
        )
      ) = s

      root_name = string.case(fst(doc))
      root = snd(doc)

      fields = ref([])
      uniqueids = ref([])

      def add_field(k, v) =
        k = string.case(k)
        v = string.trim(v)
        if v != "" then fields := file.nfo._accum_add(k, v, fields()) end
      end

      def add_uniqueid(typ, v) =
        typ = string.case(string.trim(typ))
        v = string.trim(v)
        if v != "" and typ != "" then
          uniqueids := file.nfo._accum_add(typ, v, uniqueids())
        end
      end

      def child_text(node) =
        texts =
          list.filter_map(
            fun (el) ->
              begin
                k = fst(el)
                v = snd(el)
                v = if string.case(k) == "xml_text" then v.xml_text else null end
                if null.defined(v) then
                  v = string.trim(null.get(v))
                  if v != "" then v else null end
                else
                  null
                end
              end,
            node.xml_children
          )

        list.case(texts, null, fun (x, _) -> x)
      end

      def handle_child(child) =
        name = fst(child)
        node = snd(child)
        txt = child_text(node)
        if null.defined(txt) then
          txt = null.get(txt)
          if string.case(name) == "uniqueid" then
            uid_type = list.assoc.nullable("type", node.xml_params)
            if null.defined(uid_type) then
              add_uniqueid(null.get(uid_type), txt)
            else
              add_field(name, txt)
            end
          else
            add_field(name, txt)
          end
        end
      end

      list.iter(handle_child, root.xml_children)

      md = ref([("nfo.root", root_name)])

      def emit(kv) =
        let (k, vs) = kv
        md := ("nfo." ^ k, file.nfo._join(vs))::md()
      end

      def emit_uid(kv) =
        let (typ, vs) = kv
        md := ("nfo.uniqueid." ^ typ, file.nfo._join(vs))::md()
      end

      list.iter(emit, fields())
      list.iter(emit_uid, uniqueids())
      list.rev(md())
    catch e do
      log.debug(
        label="nfo",
        "Failed to parse nfo file #{nfo_file}: #{e.kind}: #{e.message}"
      )
      []
    end
  end
end

# Enable `.nfo` sidecar metadata resolver.
#
# For a media file `.../Foo.ext`, this looks for a sibling `.../Foo.nfo`.
#
# @category Liquidsoap
# @param ~priority Resolver priority. Default is low to avoid overriding media tags.
# @param ~mime_types Restrict to these MIME types (or `null` for any).
# @param ~file_extensions Restrict to these file extensions (or `null` for any).
# @param ~populate_standard Also export a few standard keys (`title`, `year`, `genre`) when missing.
def enable_nfo_metadata(
  ~priority={0},
  ~mime_types=null,
  ~file_extensions=null,
  ~populate_standard=false
) =
  def resolver(~metadata:_, file_name) =
    nfo_file = path.remove_extension(file_name) ^ ".nfo"
    md = file.nfo.metadata(nfo_file)
    if
      populate_standard
    then
      extra = ref([])
      title = list.assoc.nullable("nfo.title", md)
      if null.defined(title) then extra := ("title", null.get(title))::extra() end
      year = list.assoc.nullable("nfo.year", md)
      if null.defined(year) then extra := ("year", null.get(year))::extra() end
      genre = list.assoc.nullable("nfo.genre", md)
      if null.defined(genre) then extra := ("genre", null.get(genre))::extra() end
      [...md, ...list.rev(extra())]
    else
      md
    end
  end

  decoder.metadata.add(
    priority=priority,
    mime_types=mime_types,
    file_extensions=file_extensions,
    "nfo",
    resolver
  )
end
