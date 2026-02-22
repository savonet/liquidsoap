# Parse XML `.nfo` sidecar files (Kodi/Emby/Jellyfin style).
#
# This is meant to be used either directly via `file.nfo.metadata(...)` or as a
# metadata resolver through `enable_nfo_metadata(...)`.

# @category File
let file.nfo = ()

# Accumulate values by key.
# @flag hidden
def file.nfo._accum_add(key, value, acc) =
  # Values are stored in reverse order for cheap cons.
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
  texts =
    list.filter_map(
      fun (child) ->
        begin
          let (name, node) = child
          if name != "xml_text" then
            null
          else
            txt = node.xml_text
            if not null.defined(txt) then
              null
            else
              txt = string.trim(null.get(txt))
              txt == "" ? null : txt
            end
          end
        end,
      children
    )

  list.case(texts, null, fun (txt, _) -> txt)
end

# @flag hidden
let file.nfo._tag_rules =
  [
    (
      "uniqueid",
      fun (node, txt) ->
        begin
          typ = list.assoc.nullable("type", node.xml_params)
          typ = null.map(fun (t) -> string.case(string.trim(t)), typ)

          if null.defined(typ) and null.get(typ) != "" then
            [("uniqueid." ^ null.get(typ), txt)]
          else
            [("uniqueid", txt)]
          end
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
                    xml_children: [(string * {xml_text: string?})]
                  }
                )
              ]
          }
        )
      ) = s

      root_name = string.case(root_name)

      def collect(fields, child) =
        let (name, node) = child
        name = string.case(name)
        txt = file.nfo._child_text(node.xml_children)

        if not null.defined(txt) then
          fields
        else
          txt = null.get(txt)
          rule = list.assoc.nullable(name, file.nfo._tag_rules)

          kvs =
            if null.defined(rule) then
              begin
                rule_fn = null.get(rule)
                rule_fn(node, txt)
              end
            else
              [(name, txt)]
            end

          def add(fields, kv) =
            let (k, v) = kv
            file.nfo._accum_add(k, v, fields)
          end

          list.fold(add, fields, kvs)
        end
      end

      fields_by_key = list.fold(collect, [], root.xml_children)

      def emit(prefix, kv) =
        let (k, vs) = kv
        (prefix ^ k, file.nfo._join(vs))
      end

      fields_md = list.map(fun (kv) -> emit("nfo.", kv), fields_by_key)

      [("nfo.root", root_name), ...fields_md]
    catch e do
      log.debug(
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
