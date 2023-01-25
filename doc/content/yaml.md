## Importing/exporting YAML values

Support for YAML parsing and rendering was first added in liquidsoap `2.2.0`. This support follows the same pattern as [JSON parsing/rendering](json.html) but using
yaml-based syntax, i.e.:

```liauidsoap
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

The only major difference being that, in YAML, all numbers are parsed and rendered as _floats_.

Please refer to the [JSON parsing and rendering](json.html) documentation for more details.
