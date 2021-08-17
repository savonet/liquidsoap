Exporting values using JSON
---------------------------

Liquidsoap can export any language value in JSON using `json.stringify`.

The format is the following :

* `() : unit` -> `[ ]`
* `true: bool` -> `true`
* `"abc" : string` -> `"abc"`
* `23 : int` -> `23`
* `2.0 : float` -> `2.0`
* `infinity : float` -> default: `null`, json5-enabled: `Infinity`
* `nan : float` -> default: `null`, json5-enabled: `NaN`
* `[2,3,4] : [int]` -> `[2,3,4]`
* `("foo",123) : string*int` -> `[ "foo", 123 ]`
* `3.14.{a = "foo", b = 24}` -> `3.14`
* `{a = "foo", b = 24}` -> `{"a": "foo", "b": 24}`
* `s : source` -> `"<source>"`
* `r : ref(int)` -> `{ "reference":4 }`
* `%mp3 : format(...)` -> ```
"%mp3(stereo,bitrate=128,samplerate=44100)"```

* `r : request(...)` -> `"<request>"`
* `f : (...)->_` -> `"<fun>"`

The specific cases to keep in mind are:

* Tuples are exported as lists, including `()`, which is an empty tuple.
* Records (unit value with decorated methods) are exported as JSON objects.
* Values decorated with methods are exported without their methods. 

Output format is pretty printed by default. A compact output can
be obtained by using the optional argument: `compact=true`.

By default, the output follows the most common `JSON` standard. However, support
for `json5` format can be enabled by passing the optional argument `json5=true`.
In particular, this allows `nan` and `infinity` to be exported to `NaN` and `Infinity`.

Generic JSON objects
--------------------

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
json.stringify(j)
- '{ "record": { "b": "ert", "a": 1 }, "key_with_methods": "value", "bla": "bar", "baz": 3.14 }'
```

Importing values using JSON
---------------------------

If compiled with `yojson` support, Liquidsoap can also
parse JSON data into values. using `json.parse`.

The format is a subset of the format of exported values with the notable
difference that only ground types (`int`, `floats`, `string`, ...)
are supported and not variable references, sources, formats,
requests and functions:

* `null` -> `() : unit`
* `true/false` -> `true/false : bool`
* `"abc"` -> `"abc" : string`
* `23` -> `23 : int`
* `2.0` -> `2.0 : float`
* `[2,3,4]` -> `[2,3,4] : int`
* `{"f": 1, "b": 4}` -> `[("f",1),("b",4)] : [(string*int)]`
* `[ "foo", 123 ]` -> `("foo",123) : string*int`

The JSON standards specify that a proper JSON payload can only be an array or an
object. However, simple integers, floats, strings and null values are
also accepted by Liquidsoap.

The function `json.parse` has the following type:

```
  (default:'a,string)->'a
```

The default parameter is very important in order to assure 
type inference of the parsed value. Its value constrains
the parser to only recognize JSON data of the the default value's 
type and is returned in case parsing fails.

Suppose that we want to receive a list of metadata, encoded as an object:

```
{ "title": "foo",
 "artist": "bar" }
```

Then, you would use json.parse with default value `[("error","fail")]` and do:

```liquidsoap
# Parse metadata from json
m = json.parse(default= [("error","fail")], json_string)
```

The type of the default value constrains the parser. For instance, in the 
above example, a JSON string `"[1,2,3,4]"` will not be accepted and the 
function will return the values passed as default.

You can use the default value in two different ways:

* To detect that the received json string was invalid/could not be parsed to the expected type. In the example above, if `json.parse` return a metadata value of `[("error","fail")]` (the default) then you can detect in your code that parsing has failed.
* As a default value for the rest of the script, if you do not want to care about parsing errors.. This can be useful for instance for JSON-RPC notifications, which should not send any response to the client anyway.

If your JSON object is of mixed type, like this one:

```
{ "uri": "https://...",
  "metadata": { "title": "foo", "artist": "bar" } }
```

You can parse it in multiple steps. For instance:

```liquidsoap
# First parse key,value list:
hint = [("key","value")]
data = json.parse(default=hint,payload)
print(data["uri"]) # "https://..."

# Then key -> (key,value) list
hint = [("list",[("key","value")])]
data = json.parse(default=hint,payload)
m    = list.assoc(default=[],"metadata",data)
print(m["title"]) # "foo"
```


