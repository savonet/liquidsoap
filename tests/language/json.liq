# We test some ground values for json import/export.

success = ref(true)

def t(x,y) =
  if x != y then
    print("Failure: #{x} instead of #{y}")
    success := false
  end
end

def test_parse_error(name, f, msg) =
  error_caught = ref(false)

  try
    print(f ())
  catch err do
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
  let json.stringify s = ()
  t(s, '[]')

  let json.stringify s = "aa'bb"
  t(s, "\"aa'bb\"")

  let json.stringify s = "a"
  t(s, '"a"')

  let json.stringify s = "©"
  t(s, '"©"')

  let json.stringify s = '"'
  t(s, '"\\""')

  let json.stringify s = '\\'
  t(s, '"\\\\"')

  let json.stringify s = (infinity:float?)
  t(s, 'null')

  let json.stringify s = (0.-infinity:float?)
  t(s, 'null')

  let json.stringify s = (nan:float?)
  t(s, 'null')

  let json.stringify s = null(infinity)
  t(s, 'null')

  let json.stringify s = null(0.-infinity)
  t(s, 'null')

  let json.stringify s = null(nan)
  t(s, 'null')

  let json.stringify[json5=true] s = infinity
  t(s, 'Infinity')

  let json.stringify[json5=true] s = (0.-infinity)
  t(s, '-Infinity')

  let json.stringify[json5=true] s = nan
  t(s, 'NaN')

  let json.stringify s = ([("foo",123), ("bar", 456)]:[(string * int)] as json.object)
  t(s, "{ \"foo\": 123, \"bar\": 456 }")

  let json.stringify s = ({foo = 123}:{ "✨foo✨" as foo: int})
  t(s, "{ \"✨foo✨\": 123 }")

  let b = json()
  b.add("b", 1)
  let json.stringify s = {
     a = null({a=1}),
     b = null(b)
  }
  t(s, "{ \"b\": { \"b\": 1 }, \"a\": { \"a\": 1 } }")

  data = "123"
  let json.parse ( x : int ) = data
  t(x, 123)

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

  t(x, {
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
  t(foo, 34.24)
  t(t1, 123)
  t(t2, 3.14)
  t(t3, false)
  t(l1, null())
  t(tl, [23, null()])

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
  t(list.assoc("aabbcc", x), 34)
  t(list.assoc("ddeerr", x), 54)

  data = '{ "foo": 123 }'
  let json.parse ( x : {
    foo : string
  }?) = data
  t(x, null())

  data = '[ "gni", 123 ]'
  let json.parse ( x : [int]? ) = data
  t(x, null())

  let json.parse ( x : (string * int * bool)? ) = data
  t(x, null())

  data = '[ "gni", 123, "gno" ]'
  let json.parse ( x : (string * int) ) = data
  t(x, ("gni",123))

  j = json()
  j.add("foo", 1)
  j.add("bla", "bar")
  j.add("baz", 3.14)
  j.add("key_with_methods", "value".{method = 123})
  j.add("record", { a = 1, b = "ert"})
  j.remove("foo")
  let json.stringify j = j
  t(j, '{
  "record": { "b": "ert", "a": 1 },
  "key_with_methods": "value",
  "bla": "bar",
  "baz": 3.14
}')

  e = ref(null())
  def f(data)
    try
      let json.stringify d = data
      ignore(d)
    catch err do
      e := err
    end
  end

  f(infinity)
  if not null.defined(!e) then success := false end

  if !success then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
