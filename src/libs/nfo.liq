# Parse XML `.nfo` sidecar files (Kodi/Emby/Jellyfin style).
#
# This is meant to be used either directly via `file.nfo.metadata(...)` or as a
# metadata resolver through `enable_nfo_metadata(...)`.

# @category File
let file.nfo = ()

# @flag hidden
def file.nfo._normalize_tag(name) =
  name = string.trim(name)
  # Strip XML namespace prefix, if any (e.g. "x:title" -> "title").
  # This also tolerates a leading ":" (malformed XML) by stripping it.
  name = r/^[^:]*:/.replace(fun (_) -> "", name)
  string.case(name)
end

# Accumulate values by key.
# @flag hidden
def file.nfo._accum_add(key, value, acc) =
  # Values are stored in reverse order for cheap cons.
  # Assoc-list update is O(n) in distinct keys (NFOs are typically small).
  def rec aux(acc) =
    list.case(
      acc,
      [(key, [value])],
      fun (kv, rest) ->
        if fst(kv) == key then
          (key, value::snd(kv))::rest
        else
          kv::aux(rest)
        end
    )
  end

  aux(acc)
end

# @flag hidden
def file.nfo._join(vs) =
  string.concat(separator=";", list.rev(vs))
end

# @flag hidden
def file.nfo._child_text(children) =
  # Return the first non-empty trimmed text node, if any.
  texts =
    list.filter_map(
      fun (child) ->
        begin
          let (name, node) = child
          if name != "xml_text" then
            null
          else
            txt = string.trim(node.xml_text)
            txt == "" ? null : txt
          end
        end,
      children
    )

  list.case(texts, null, fun (txt, _) -> txt)
end

# @flag hidden
let file.nfo._tag_rules =
  # Dispatch table for tags needing special handling.
  # Shape: [(tag_name, (node, text) -> [(key, value)])].
  [
    (
      "uniqueid",
      fun (node, txt) ->
        begin
          typ = list.assoc.nullable("type", node.xml_params)
          typ =
            null.map(
              fun (t) ->
                begin
                  t = string.case(string.trim(t))
                  t == "" ? null : t
                end,
              typ
            )

          null.case(
            typ,
            {[ ("uniqueid", txt) ]},
            fun (t) -> [("uniqueid." ^ t, txt)]
          )
        end
    )
  ]

# Extract metadata from an `.nfo` file.
#
# Returned metadata keys are namespaced under `nfo.*`.
#
# Currently, only direct children of the root element that contain textual
# content are extracted.
#
# Special case:
# - `<uniqueid type="X">VALUE</uniqueid>` is exported as `nfo.uniqueid.X=VALUE`.
# - Repeated tags are joined with `;` (in order of appearance).
# - Tag names and unique ID types are lowercased; namespace prefixes are ignored.
#
# @category File
# @param nfo_file Path to the `.nfo` file.
def file.nfo.metadata(nfo_file) =
  if
    not (file.exists(nfo_file))
  then
    []
  else
    try
      s = file.contents(nfo_file)
      let xml.parse ((root_name, root) :
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
                    xml_children: [(string * {xml_text: string})]
                  }
                )
              ]
          }
        )
      ) = s

      root_name = file.nfo._normalize_tag(root_name)

      def collect(fields, child) =
        let (name, node) = child
        name = file.nfo._normalize_tag(name)
        txt = file.nfo._child_text(node.xml_children)

        if not null.defined(txt) then
          fields
        else
          txt = null.get(txt)
          rule = list.assoc.nullable(name, file.nfo._tag_rules)

          kvs =
            null.case(rule, {[(name, txt)]}, fun (rule_fn) -> rule_fn(node, txt))

          def add(fields, kv) =
            let (k, v) = kv
            file.nfo._accum_add(k, v, fields)
          end

          list.fold(add, fields, kvs)
        end
      end

      fields_by_key = list.fold(collect, [], root.xml_children)

      fields_md =
        list.map(
          fun (kv) ->
            begin
              let (k, vs) = kv
              ("nfo." ^ k, file.nfo._join(vs))
            end,
          fields_by_key
        )

      [("nfo.root", root_name), ...fields_md]
    catch e do
      log.info(
        label="file.nfo.metadata",
        "Failed to parse nfo file #{string.quote(nfo_file)}: #{e.kind}: #{
          e.message
        }"
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
def enable_nfo_metadata(
  ~priority={0},
  ~mime_types=null,
  ~file_extensions=null
) =
  def resolver(~metadata:_, file_name) =
    nfo_file = path.remove_extension(file_name) ^ ".nfo"
    file.nfo.metadata(nfo_file)
  end

  decoder.metadata.add(
    priority=priority,
    mime_types=mime_types,
    file_extensions=file_extensions,
    "nfo",
    resolver
  )
end
