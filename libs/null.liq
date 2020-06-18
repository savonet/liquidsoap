# Determine whether a nullable value is null.
# @category Liquidsoap
def null.is(x) =
  null.case(x, {true}, fun (_) -> false)
end