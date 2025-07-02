data =
  '{
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

let json.parse (x :
  {
    foo: float,
    "gni gno" as gni_gno: bool,
    nested: {
      tuple: (_ * float),
      list: [float],
      nullable_list: [int?],
      object_as_list: [(string*float)] as json.object,
      "arbitrary object key ✨" as arbitrary_object_key: bool,
      not_present: bool?
    }
  }
) = data
