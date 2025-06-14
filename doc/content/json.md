## Importing JSON values

_Note:_ If you are reading this page for the first time, you might want to skip directly to the
explicit type annotation below as this is the recommended way of parsing JSON data. The content
before that is here to explain the inner workings of JSON parsing in `liquidsoap`.

Liquidsoap supports importing JSON values through a special `let` syntax. Using this syntax
makes it relatively natural to parse JSON data in your script while keeping type-safety at runtime.
Here's an example:

```{.liquidsoap include="json1.liq"}

```

This prints:

```
We parsed a JSON object and got value abc for attribute foo!
```

What happened here is that liquidsoap kept track of the fact that `v` was called with
`v.foo` and that the result of that was a string. Then, at runtime, it checks the parsed
JSON value against this type and raises an issue if that did not match. For instance,
the following script:

```liquidsoap
let json.parse v = '{"foo": 123}'

print("We parsed a JSON object and got value " ^ v.foo ^ " for attribute foo!")
```

raises the following exception:

```
Error 14: Uncaught runtime error:
type: json,
message: "Parsing error: json value cannot be parsed as type {foo: string, _}"
```

Of course, this all seems pretty trivial presented like that but, let's switch to reading a file instead:

```liquidsoap
let json.parse v = file.contents("/path/to/file.json")

print("We parsed a JSON object and got value " ^ v.foo ^ " for attribute foo!")
```

Now, this is getting somewhere! Let's push it further and parse a whole `package.json` from
a typical `npm` package:

```liquidsoap
# Content of package.json is:
# {
#  "name": "my_package",
#  "version": "1.0.0",
#  "scripts": {
#    "test": "echo \"Error: no test specified\" && exit 1"
#  },
#  ...
let json.parse package = file.contents("/path/to/package.json")

name = package.name
version = package.version
test = package.scripts.test

print("This is package " ^  name ^ ", version " ^ version ^ " with test script: " ^ test)
```

And we get:

```
This is package my_package, version 1.0.0 with test script: echo "Error: no test specified" && exit 1
```

This can even be combined with _patterns_:

```liquidsoap
let json.parse {
  name,
  version,
  scripts = {
    test
  }
} = file.contents("/path/to/package.json")

print("This is package " ^  name ^ ", version " ^ version ^ " with test script: " ^ test)
```

Now, this is looking nice!

## Explicit type annotation

Explicit type annotation are the recommended way to parse JSON data.

Let's try a slight variation of the previous script now:

```liquidsoap
let json.parse {
  name,
  version,
  scripts = {
    test
  }
} = file.contents("/path/to/package.json")

print("This is package #{name}, version #{version} with test script: #{test}")
```

This returns:

```
This is package null, version null with test script: null
```

What? 🤔

This is because, in this script, we only use `name`, `version`, etc.. through the interpolation syntax `#{...}`. However, interpolated
variables can be anything so this does not leave enough information to the typing system to know what type those variables should be and,
in this case, we default to `null`.

In order to avoid bad surprises like this, it is usually recommended to add **type annotations** to your json parsing call
to explicitly state what kind of data you are expecting. Let's add one here:

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

print("This is package #{name}, version #{version} with test script: #{test}")
```

And we get:

```
This is package my_package, version 1.0.0 with test script: echo "Error: no test specified" && exit 1
```

Back to normal!

### Type syntax

The syntax for type annotation is as follows:

#### Ground types

`string`, `int`, `float` are parsed as, resp., a string, an integer or a floating point number. Note that if your json value contains an integer such as `123`, parsing it as a floating point number will succeed. Also, if an integer is too big to be represented as an `int` internally, it will be parsed as a floating point number.

#### Nullable types

All type annotation can be postfixed with a trailing `?` to denote a _nullable_ value. If a type is nullable, the json parser will return `null` when it cannot parse
the value as the principal type. This is particularly useful when you are not sure of all the types that you are parsing.

For instance, some `npm` packages do not have a `scripts` entry or a `test` entry, so you would parse them as:

```liquidsoap
let json.parse ({
  name,
  version,
  scripts,
} : {
  name: string,
  version: string,
  scripts: {
    test: string?
  }?
}) = file.contents("/path/to/package.json")
```

And, later, inspect the returned value to see if it is in fact present. You can do it in several ways:

```liquidsoap
# Check if the value is defined:
test =
  if null.defined(scripts) then
    null.get(scripts.test)
  else
    null ()
  end

# Use the ?? syntax:
test = (scripts ?? { test = null }).test
```

#### Tuple types

The type `(int * float * string)` tells liquidsoap to parse a JSON array whose _first values_ are of type: `int`, `float` and `string`. If any further values
are present in the array, they will be ignored.

For arrays as well as any other structured types, the special notation `_` can be used to denote any type. For instance, `(_ * _ * float)` denotes an JSON
array whose first 2 elements can be of any type and its third element is a floating point number.

#### Lists

The type `[int]` tells liquidsoap to parse a JSON array where _all its values_ are integers as a list of integers. If you are not sure if all elements in the
array are integers, you can always use nullable integers: `[int?]`

#### Objects

The type `{foo: int}` tells liquidsoap to parse a JSON object as a record with an attribute labelled `foo` whose value is an integer. All other
attributes are ignored.

Arbitrary object keys can be parsed using the following syntax: `{"foo bar key" as foo_bar_key: int}`, which tells liquidsoap to parse a JSON object
as a record with an attribute labelled `foo_bar_key` which maps to the attribute `"foo bar key"` from the JSON object.

#### Associative lists as objects

It can sometimes be useful to parse a JSON object as an associative list, for instance if you do not know in advance all the possible keys of
an object. In this case, you can use the special type: `[(string * int)] as json.object`. This tells liquidsoap to parse the JSON object as a list
of pairs `(string * int)` where `string` represents the attribute label and `int` represent the attribute value.

If you are not sure if all the object values are integers you can always use nullable integers: `[(string * int?)] as json.object`

### Parsing errors

When parsing fails, a `error.json` is raised which can be caught at runtime:

```liquidsoap
try
   let json.parse ({
      status,
      data = {
        track
      }
    } : {
      status: string,
      data: {
        track: string
      }
    }) = res

    # Do something on success here..
catch err: [error.json] do
  # Do something on parse errors here..
end
```

#### Example

Here's a full example. Feel free to refer to `tests/language/json.liq` in the source code for more of them.

```{.liquidsoap include="json-ex.liq"}

```

It returns

```
  - x : {
    foo = 34.24,
    gni_gno = true,
    nested = {
      tuple = (null, 3.14),
      list = [44., 55., 66.12],
      nullable_list = [null, 23, null],
      object_as_list = [("foo", 123.), ("gni", 456.0), ("gno", 3.14)],
      arbitrary_object_key = true,
      not_present = null
    }
  }
```

### JSON5 extension

Liquidsoap supports the [JSON5](https://json5.org/) extension. Parsing of `json5` values is enabled with the following argument:

```liquidsoap
let json.parse[json5=true] x = ...
```

If a `json5` variable is in scope, you can also simply use `let json.parse[json5] x = ...`

## Exporting JSON values

Exporting JSON values can be done using the `json.stringify` function:

```{.liquidsoap include="json-stringify.liq"}

```

Please note that not all values are exportable as JSON, for instance function. In such cases the function will raise an `error.json` exception.

## Generic JSON objects

Generic `JSON` objects can be manipulated through the `json()` operator. This operator
returns an opaque json variable with methods to `add` and `remove` attributes:

```liquidsoap
j = json()
j.add("foo", 1)
j.add("bla", "bar")
j.add("baz", 3.14)
j.add("key_with_methods", "value".{method = 123})
j.add("record", { a = 1, b = "ert"})
j.remove("foo")
s = json.stringify(j)
- s: '{ "record": { "b": "ert", "a": 1 }, "key_with_methods": "value", "bla": "bar", "baz": 3.14 }'
```
