# JSON

Liquidsoap can parse and generate JSON data directly in your scripts. You can
use it to load configuration files, talk to web APIs or exchange metadata with
other programs.

This page explains how JSON parsing works, how types drive the parser, and how
to use nullable types, custom keys and objects parsed as lists.

## Parsing a JSON object

You can parse a JSON string using the special `let json.parse` syntax:

```liquidsoap
let json.parse v = '{"foo": "abc"}'
print("We parsed a JSON object and got value " ^ v.foo ^ " for attribute foo!")
```

This prints:

```
We parsed a JSON object and got value abc for attribute foo!
```

Liquidsoap infers from the script that `v.foo` is used as a string. At runtime,
the parser checks that the JSON data contains a `"foo"` field and that its value
is a string. If the data does not match, the parser raises an error.

For instance, with a number in place of the string:

```liquidsoap
let json.parse v = '{"foo": 123}'
```

the script fails with:

```
Error 14: Uncaught runtime error:
type: json,
message: "Parsing error: json value cannot be parsed as type {foo: string, _}"
```

## Loading JSON from files

You can load the JSON data from a file:

```liquidsoap
let json.parse v = file.contents("/path/to/file.json")
```

For instance, to parse the `package.json` file of an npm package:

```liquidsoap
let json.parse package = file.contents("/path/to/package.json")

name = package.name
version = package.version
test = package.scripts.test

print("This is package " ^ name ^ ", version " ^ version ^ " with test script: " ^ test)
```

## Type annotations

Liquidsoap sometimes cannot infer the type of a parsed value. This happens for
instance when a variable is only used inside a string interpolation (`#{...}`).
In this case, the value is parsed as `null` and Liquidsoap logs a warning.

To avoid this, add a type annotation to the parse statement. The parser uses the
annotation to decide which JSON data to extract:

```liquidsoap
let json.parse ({
  name,
  version,
  scripts = {
    test
  }
} : {
  name: string,
  version: string,
  scripts: {
    test: string
  }
}) = file.contents("/path/to/package.json")
```

With this annotation, `name`, `version` and `test` are strings, even when you
only use them inside interpolations.

The following sections describe the types you can use in annotations.

### Ground types

| Type     | Description                  | Example value     |
| -------- | ---------------------------- | ----------------- |
| `string` | A sequence of characters     | `"hello"`         |
| `int`    | An integer                   | `42`              |
| `float`  | A number, including decimals | `3.14` or `123.0` |
| `bool`   | A boolean                    | `true`            |

Integers are accepted where a `float` is expected: `123` can be parsed as a
`float`.

### Nullable types

Add `?` to a type to make it nullable:

```liquidsoap
test: string?  # test is either a string or null
```

A nullable field is set to `null` when it is missing from the JSON data or when
its value has a different type. Use nullable types for data that may or may not
include a field:

```liquidsoap
let json.parse ({
  scripts
} : {
  scripts: {
    test: string?
  }?
}) = file.contents("package.json")
```

You can then check whether the values are present:

```liquidsoap
# Option 1: Explicit check
test =
  if null.defined(scripts) then
    null.get(scripts).test
  else
    null
  end

# Option 2: Fallback value
test = (scripts ?? { test = null }).test
```

### Tuples

A tuple type parses a JSON array with a specific type for each position:

```liquidsoap
(int * float * string)
```

This parses a JSON array like `[1, 2.5, "hello"]`.

Use `_` for the positions you want to skip:

```liquidsoap
(_ * _ * float)  # Only the third element must be a float
```

### Lists

A list type parses a JSON array whose elements all have the same type:

```liquidsoap
[int]     # list of integers
[float?]  # list of nullable floats
```

For instance, the JSON array

```json
[44.0, 55, 66.12]
```

can be parsed with type `[float]`.

### Objects

A record type parses a JSON object into named fields:

```liquidsoap
{foo: int, bar: string}
```

The parser extracts the fields listed in the type and ignores the other fields
of the JSON object.

### Custom JSON keys

JSON keys can contain spaces and other characters that are not valid in
Liquidsoap variable names. Use `as` to map such a key to a field name:

```liquidsoap
{"foo bar" as foo_bar: int}
```

With this type, the JSON object

```json
{ "foo bar": 123 }
```

is parsed as a record with field `foo_bar = 123`.

### Associative objects as lists

When you do not know the keys of an object in advance, parse the object as a list
of key/value pairs with the type `[(string * <value type>)] as json.object`.

For instance, the JSON object

```json
{ "a": 1, "b": 2, "c": 3 }
```

parsed with type

```liquidsoap
[(string * int)] as json.object
```

gives the list

```liquidsoap
[("a", 1), ("b", 2), ("c", 3)]
```

Use `int?` as the value type if some values may be missing or of another type:
these values are parsed as `null`.

## Handling errors

A parsing error raises an `error.json` error, which you can catch:

```liquidsoap
try
  let json.parse ({status, data = {track}} : {...}) = response
  # Do something with data
catch err: [error.json] do
  # Handle the parse failure
end
```

## Full example

The following example uses all the types described above:

```{.liquidsoap include="json-ex.liq"}

```

## Other features

### JSON5

Liquidsoap can parse [JSON5](https://json5.org/) data, which allows trailing
commas, comments and other extensions:

```liquidsoap
let json.parse[json5=true] x = ...
```

### Exporting to JSON

`json.stringify` converts a value to a JSON string:

```liquidsoap
print(json.stringify({artist="Bla", title="Blo"}))
```

### Building JSON manually

When you generate JSON output dynamically, use `json.object` to build an object
key by key:

```liquidsoap
j = json.object()
j.add("foo", 1)
j.add("bar", "baz")
j.remove("foo")
print(json.stringify(j))
```

`json.value` converts any value to the `json` type. You can use it when a
function returns JSON values of different types, for instance in an HTTP
response handler:

```liquidsoap
try
  # Send a number here:
  id = 1234
  res.json(json.value(id))
catch err do
  # Or a string in case of an error:
  res.json(json.value("Error while processing request: #{err}"))
end
```

The test file `tests/language/json.liq` in the Liquidsoap source repository
contains many more examples.
