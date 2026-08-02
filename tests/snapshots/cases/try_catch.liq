try
  error.raise(error.not_found, "nope")
catch err : [error.not_found] do
  err.message
end
