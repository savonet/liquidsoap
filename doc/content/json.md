## 📦 Working with JSON in Liquidsoap

Liquidsoap makes it easy—and safe—to work with JSON data directly in your scripts. Whether you're loading configuration files, interfacing with APIs, or managing playlist metadata, JSON is a powerful format to master.

This page walks you through how JSON parsing works in Liquidsoap, how type safety is enforced, and how to use advanced features like nullable types, custom keys, and associative object parsing.

Let’s start simple and build progressively toward the more advanced features. 🧗

### 🔹 Getting Started: Parsing a Simple JSON Object

You can parse a JSON string using the special `let json.parse` syntax:

```liquidsoap
let json.parse v = '{"foo": "abc"}'
print("We parsed a JSON object and got value " ^ v.foo ^ " for attribute foo!")
```

✅ Output:

```
We parsed a JSON object and got value abc for attribute foo!
```

What's happening here?

Liquidsoap watches how you use `v.foo` (as a string), and it _checks at runtime_ that the JSON contains `"foo"` and that its value is indeed a string. If there's a mismatch, you'll get a clear error.

Example with incorrect type:

```liquidsoap
let json.parse v = '{"foo": 123}'
```

⛔ Raises:

```
Error 14: Uncaught runtime error:
type: json,
message: "Parsing error: json value cannot be parsed as type {foo: string, _}"
```

### 📁 Loading JSON from Files

Instead of hardcoding JSON as a string, you can load it from a file:

```liquidsoap
let json.parse v = file.contents("/path/to/file.json")
```

Let’s look at a realistic example. Suppose you’re parsing a `package.json` file from an npm package:

```liquidsoap
let json.parse package = file.contents("/path/to/package.json")

name = package.name
version = package.version
test = package.scripts.test

print("This is package " ^ name ^ ", version " ^ version ^ " with test script: " ^ test)
```

### ✅ Use Type Annotations for Reliable Parsing

Sometimes Liquidsoap can’t infer types correctly—especially when you use variables only inside string interpolations (`#{...}`). In those cases, values default to `null`, which might be confusing.

The solution: add an **explicit type annotation** to your parse statement. The type annotation is then used to drive the parser and pick the json data that you are expecting:

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

Now everything works as expected—even if you only reference variables inside interpolations.

## 🧩 Understanding JSON Type Annotations

Liquidsoap’s JSON parser uses a rich type system that maps onto JSON’s structure. Let’s break it down:

### **🔤 Ground Types**

| Type     | Description                  | Example value     |
| -------- | ---------------------------- | ----------------- |
| `string` | A sequence of characters     | `"hello"`         |
| `int`    | An integer                   | `42`              |
| `float`  | A number, including decimals | `3.14` or `123.0` |

Liquidsoap will coerce integers into floats if needed (e.g. `123` can be a `float`).

### **❓ Nullable Types**

Add `?` to make a type optional:

```liquidsoap
test: string?  # test is either a string or null
```

Useful when parsing data that may or may not include a field:

```liquidsoap
let json.parse ({
  scripts
} : {
  scripts: {
    test: string?
  }?
}) = file.contents("package.json")
```

You can check for presence using:

```liquidsoap
# Option 1: Explicit check
test =
  if null.defined(scripts) then
    null.get(scripts.test)
  else
    null()
  end

# Option 2: Fallback value
test = (scripts ?? { test = null }).test
```

---

### **🔗 Tuples**

Tuples parse fixed-size arrays with specific types for each position:

```liquidsoap
(int * float * string)
```

This parses a JSON array like `[1, 2.5, "hello"]`.

Use `_` as a wildcard to ignore types you don’t care about:

```liquidsoap
(_ * _ * float)  # Only the third element must be a float
```

---

### **📋 Lists**

To parse a JSON array of values of the same type, use brackets:

```liquidsoap
[int]     # list of integers
[float?]  # list of optional floats
```

Example:

```json
[44.0, 55, 66.12]
```

Can be parsed as: `[float]`

### **🧱 Objects (Records)**

Use `{...}` to parse JSON objects into named fields:

```liquidsoap
{foo: int, bar: string}
```

This tells Liquidsoap to extract only the fields you care about. Extra fields in the JSON are ignored.

### 🏷️ **Custom JSON Keys**

JSON keys often contain characters or spaces that aren't valid Liquidsoap variable names.

You can map them like this:

```liquidsoap
{"foo bar" as foo_bar: int}
```

Example:

```json
{ "foo bar": 123 }
```

Liquidsoap parses this as a variable `foo_bar = 123`.

### 🗂️ **Associative Objects as Lists**

What if you don’t know the keys in advance?

Use `[(string * < value type>)] as json.object` to treat an object like a list of key-value pairs.

Example JSON:

```json
{ "a": 1, "b": 2, "c": 3 }
```

Use this type:

```liquidsoap
[(string * int)] as json.object
```

Parsed as:

```liquidsoap
[("a", 1), ("b", 2), ("c", 3)]
```

You can even use `int?` if some values might be missing or of a non-int type.

### ⚠️ Handling Errors

Parsing errors raise a `error.json` exception:

```liquidsoap
try
  let json.parse ({status, data = {track}} : {...}) = response
  # Do something with data
catch err: [error.json] do
  # Handle the parse failure
end
```

---

## 🧪 Full Example

```liquidsoap
data = '{
  "foo": 34.24,
  "gni gno": true,
  "nested": {
    "tuple": [123, 3.14, false],
    "list":  [44.0, 55, 66.12],
    "nullable_list": [12.33, 23, "aabb"],
    "object_as_list": {
      "foo": 123,
      "gni": 456.0,
      "gno": 3.14
    },
    "arbitrary object key ✨": true
  }
}'

let json.parse (x :
  {
    foo: float,
    "gni gno" as gni_gno: bool,
    nested: {
      tuple: (_ * float),
      list: [float],
      nullable_list: [int?],
      object_as_list: [(string * float)] as json.object,
      "arbitrary object key ✨" as arbitrary_key: bool,
      not_present: bool?
    }
  }
) = data
```

### 🛠️ Other Features

- **JSON5 support** (for trailing commas, comments, etc.):

```liquidsoap
let json.parse[json5=true] x = ...
```

- **Exporting to JSON**:

```liquidsoap
print(json.stringify({artist="Bla", title="Blo"}))
```

- **Building JSON manually**:

This can be useful when dynamically generating json output:

```liquidsoap
j = json()
j.add("foo", 1)
j.add("bar", "baz")
j.remove("foo")
print(json.stringify(j))
```

## 🚀 Recap

✅ Liquidsoap gives you a safe and expressive way to work with JSON
🧠 Type annotations help catch issues early and make your code clearer
🛠️ Advanced types let you tackle real-world data with ease

Once you’ve got the hang of parsing, try exploring the actual `tests/language/json.liq` test file in the source repo—it's full of neat examples and tricks!
