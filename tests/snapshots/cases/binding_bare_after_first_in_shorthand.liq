# The record ambiguity only affects the *first* statement of a `{ ... }` body.
# A leading `x = 1` would be the record `{ x = 1 }`, but once past it a bare
# binding is fine. src/libs/extra/fades.liq relies on this.
f =
  {
    let a = 1
    b = a + 1
    b
  }
f()
