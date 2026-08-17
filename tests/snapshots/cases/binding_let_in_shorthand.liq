# The form `{ … }` does accept: `let`, unlike a bare binding.
f =
  {
    let x = 1
    x + 1
  }
f()
