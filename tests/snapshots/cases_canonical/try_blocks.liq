# `try` is emitted as a `parts` array of try_body / try_catch / try_finally,
# each with its own keyword-anchored span.
try
  error.raise(error.not_found, "nope")
catch err : [error.not_found] do
  err.message
finally
  ()
end
