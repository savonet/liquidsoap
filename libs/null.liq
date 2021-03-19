# Determine whether a nullable value is not null.
# @category Liquidsoap
def null.defined(x) =
  null.case(x, {false}, fun (_) -> true)
end

# Convert a nullable value to a list containing zero or one element depending on
# whether the value is null or not.
# @category Liquidsoap
def null.to_list(x)
  null.case(x, {[]}, fun (x) -> [x])
end
