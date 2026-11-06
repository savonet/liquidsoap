%include "../test.liq"

def typechecks(expr) =
  try
    let eval _ = expr
    true
  catch _ do
    false
  end
end

# Each rejected expression has an accepted twin with the same syntax, so a
# parse error cannot pass for a type error.
def accepted(expr) =
  if not typechecks(expr) then test.fail("should typecheck: #{expr}") end
end

def rejected(expr) =
  if typechecks(expr) then test.fail("should not typecheck: #{expr}") end
end

# Top-level bindings after an annotated one stay visible to `let eval`.
typeof_base = 1
let (typeof_annotated : %typeof(typeof_base)) = 3
typeof_after = typeof_annotated + 5

def f() =
  let eval after = "typeof_after + 1"
  test.equal(after, 9)

  # The type of a reference can be pinned after the annotation that uses it.
  data = ref([])
  let json.parse (saved : %typeof(data())) = '[{"id": 3}]'
  data := saved
  data := [{id = 1}]
  test.equal(saved, [{id = 3}])

  # The term of %typeof is never evaluated.
  evaluated = ref(false)
  def boom() =
    evaluated := true
    [1]
  end
  let json.parse (l : %typeof(boom())) = "[2]"
  test.equal(l, [2])
  test.equal(evaluated(), false)

  # Annotating with the type of a polymorphic value keeps generalization.
  generic = ([] : %typeof([]))
  test.equal(list.add(1, generic), [1])
  test.equal(list.add("a", generic), ["a"])
  accepted(
    '(items : %typeof([])) = [] ignore(list.add(1, items)) ignore(list.add("a", items))'
  )

  # Argument annotations.
  x = 1
  def incr((v : %typeof(x))) =
    v + 1
  end
  test.equal(incr(2), 3)

  # Nested annotations and methods.
  r = {foo = 1.5, bar = "x"}
  let json.parse (z :
    {a : %typeof(r.foo), b : %typeof(r).bar, c : [%typeof(x)]?}
  ) = '{"a": 2.5, "b": "q", "c": [4]}'
  test.equal(z.a, 2.5)
  test.equal(z.b, "q")
  test.equal(z.c, [4])

  # The annotation is the exact type, bound before the annotated term is checked.
  accepted("ignore((1 : %typeof(1)))")
  rejected("ignore((null : %typeof(1)))")
  accepted("ignore(((1. : %typeof(1.)) : float))")
  rejected("ignore(((1 : %typeof(1.)) : int))")
  accepted('def g((v : %typeof(1))) = v end ignore(g(1))')
  rejected('def g((v : %typeof(1))) = v end ignore(g("a"))')

  accepted(
    'y = 1 def g(~a=1, (b : %typeof(y))) = a + b end def h(%argsof(g)) = g(%argsof(g), 2) end ignore(h(a=3))'
  )

%ifdef sqlite
  # The `sqlite.query` reducer constrains the annotated type before typechecking.
  accepted(
    'def q(db) = let sqlite.query (rows : %typeof([{n=1}])) = db.select("SELECT 1 AS n") rows end ignore(q)'
  )
%endif

  test.pass()
end

test.check(f)
