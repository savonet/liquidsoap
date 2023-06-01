# We test some ground values for json import/export.

def test_parse_error(name, f, msg) =
  error_caught = ref(false)

  try
    print(f ())
  catch err: [error.json] do
    if err.kind != "json" then
      print("parse error test #{name} failed: wrong error kind, got: #{err.kind}, expected: json")
      test.fail()
    end
    if err.message != msg then
      print("parse error test #{name} failed: wrong error message, got: #{err.message}, expected: #{msg}")
      test.fail()
    end
    error_caught := true
  end

  if not !error_caught then
    print("parse error test #{name} failed: no error caught")
    test.fail()
  end
end

def f() =
  test.equals(json.stringify(()), '[]')
  test.equals(json.stringify("aa'bb"), "\"aa'bb\"")
  test.equals(json.stringify("a"), '"a"')
  test.equals(json.stringify("©"), '"©"')
  test.equals(json.stringify('"'), '"\\""')
  test.equals(json.stringify('\\'), '"\\\\"')

  test.equals(json.stringify(json5=true, infinity), 'Infinity')
  test.equals(json.stringify(json5=true, (0.-infinity)), '-Infinity')
  test.equals(json.stringify(json5=true,nan), 'NaN')

  let b = json()
  b.add("b", 1)
  s = json.stringify({
     a = null({a=1}),
     b = null(b)
  })
  test.equals(s, "{ \"a\": { \"a\": 1 }, \"b\": { \"b\": 1 } }")

  data = "123"
  let json.parse ( x : int ) = data
  test.equals(x, 123)

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
     },
     "extra": "ignored"
  }'

  let json.parse ( x : {
    foo : float,
    "gni gno" as gni_gno : bool,
    nested : {
      tuple : (_ * float),
      list : [float],
      nullable_list : [int?],
      object_as_list : [(string * float)] as json.object,
      "arbitrary object key ✨" as arbitrary_object_key : bool,
      not_present : bool?
    }
  }) = data

  test.equals(x, {
    foo = 34.24,
    gni_gno = true,
    nested = {
      tuple = (null(), 3.14),
      list = [44., 55., 66.12],
      nullable_list = [null(), 23, null()],
      object_as_list = [("foo", 123.), ("gni", 456.0), ("gno", 3.14)],
      arbitrary_object_key = true,
      not_present = null()
    }
  })

  # Pattern extraction with json parsing
  let json.parse {
    foo,
    nested = {
      tuple = (t1, t2, t3),
      nullable_list = [l1, ...tl]
    }
  } = data
  test.equals(foo, 34.24)
  test.equals(t1, 123)
  test.equals(t2, 3.14)
  test.equals(t3, false)
  test.equals(l1, null())
  test.equals(tl, [23, null()])

  let json.parse x = data
  ignore(x.foo + 1.0)
  let (x, y, _) = x.nested.tuple
  ignore(x + 1)
  ignore(y + 3.14)

  def failed_array () =
    data = "[]"
    let json.parse (val : { x : int }) = data
    print(val.x)
  end

  test_parse_error(
    "failed array parsing",
    failed_array,
    "Parsing error: json value cannot be parsed as type {x : int}"
  )

  def failed_runtime () =
    let json.parse x = data
    ignore(x.foo + 1.0)
    let (x, _, _) = x.nested.tuple
    ignore(x ^ "foo")
  end

  test_parse_error(
    "failed runtime",
    failed_runtime,
    "Parsing error: json value cannot be parsed as type {nested: {tuple: (string,_,_), _}, _}"
  )


  def nested_tuple () =
    let json.parse ( x : {
      nested : {
        tuple : (int * float * int * bool),
        list : [float],
        nullable_list : [int?],
        object_as_list : [(string * float)] as json.object,
        "arbitrary object key ✨" as arbitrary_object_key : bool,
        not_present : bool?
      }
    }) = data
    ignore(data)
  end

  test_parse_error(
    "nested tuple",
    nested_tuple,
    "Parsing error: json value cannot be parsed as type {nested: {tuple: (_,_,int,_), _}, _}"
  )

  def nested_list() =
    let json.parse ( x : {
      nested : {
        tuple : (int * float * bool),
        list : [int],
        nullable_list : [int?],
        object_as_list : [(string * float)] as json.object,
        "arbitrary object key ✨" as arbitrary_object_key : bool,
        not_present : bool?
      }
    }) = data
    ignore(data)
  end

  test_parse_error(
    "nested list",
    nested_list,
    "Parsing error: json value cannot be parsed as type {nested: {list: [int], _}, _}"
  )

  def nested_object() =
    let json.parse ( x : {
      nested : {
        tuple : (int * float * bool),
        list : [float],
        nullable_list : [int],
        object_as_list : [(string * float)] as json.object,
        "arbitrary object key ✨" as arbitrary_object_key : bool,
        not_present : bool?
      }
    }) = data
    ignore(data)
  end

  test_parse_error(
    "nested object",
    nested_object,
    'Parsing error: json value cannot be parsed as type {nested: {nullable_list: [int], _}, _}'
  )

  data = '{"aabbcc": 34, "ddeerr": 54 }'
  let json.parse (x : [(string * int)] as json.object) = data
  test.equals(list.assoc("aabbcc", x), 34)
  test.equals(list.assoc("ddeerr", x), 54)

  data = '{ "foo": 123 }'
  let json.parse ( x : {
    foo : string
  }?) = data
  test.equals(x, null())

  data = '[ "gni", 123 ]'
  let json.parse ( x : [int]? ) = data
  test.equals(x, null())

  let json.parse ( x : (string * int * bool)? ) = data
  test.equals(x, null())

  data = '[ "gni", 123, "gno" ]'
  let json.parse ( x : (string * int) ) = data
  test.equals(x, ("gni",123))

  data = '{
    "foo": {
      "gni": {
        "bla": 123
      }
    }
  }'
  let json.parse x = data
  test.equals(x?.foo.gni?.bla, null(123))

  data = '{
    "foo": {}
  }'
  let json.parse x = data
  test.equals(x?.foo.gni?.bla, null())

  data = '{}'
  let json.parse x = data
  test.equals(x?.foo?.gni?.bla, null())

  # Test escaping of invalid utf8 strings.
  s = "S\x00e\x00k\x00e\x00r\x00n\x00u\x00p\x00 \x00K\x00i\x00n\x00g\x00e\x00r\x00p\x00\xE2\x00"
  test.equals(json.stringify(s), "\"S\\u0000e\\u0000k\\u0000e\\u0000r\\u0000n\\u0000u\\u0000p\\u0000 \\u0000K\\u0000i\\u0000n\\u0000g\\u0000e\\u0000r\\u0000p\\u0000\\uFFFD\"")

  j = json()
  j.add("foo", 1)
  j.add("bla", "bar")
  j.add("baz", 3.14)
  j.add("key_with_methods", "value".{method = 123})
  j.add("record", { a = 1, b = "ert"})
  j.remove("foo")
  j = json.stringify(j)
  test.equals(j, '{
  "record": { "a": 1, "b": "ert" },
  "key_with_methods": "value",
  "bla": "bar",
  "baz": 3.14
}')

  e = ref(null())
  def f(data)
    try
      d = json.stringify(data)
      ignore(d)
    catch err do
      e := err
    end
  end

  f(infinity)
  if not null.defined(!e) then test.fail() end

  test.pass()
end

test.check(f)
