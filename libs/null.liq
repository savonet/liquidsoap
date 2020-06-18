# Determine whether a nullable value is not null.
# @category Liquidsoap
def null.defined(x) =
  null.case(x, {false}, fun (_) -> true)
end
