## Importing/exporting YAML values

Support for YAML parsing and rendering was first added in liquidsoap `2.2.0`. This support follows the same pattern as [JSON parsing/rendering](./json.md) but using
yaml-based syntax, i.e.:

```liquidsoap
let yaml.parse ({
  name,
  version,
  scripts,
} : {
  name: string,
  version: string,
  scripts: {
    test: string?
  }?
}) = file.contents("/path/to/file.yaml")
```

and

```liquidsoap
r = {artist = "Bla", title = "Blo"}
print(yaml.stringify(r))
```

The only major differences being that YAML numbers are always rendered as _floats_, and that the
supported YAML subset is the one usually hand-written in configuration files: block mappings and
sequences, inline flow collections, plain and quoted scalars and comments. Block scalars (`|` and
`>`), anchors, aliases, tags and multi-document streams are rejected with an explicit error.

Please refer to the [JSON parsing and rendering](./json.md) documentation for more details.
