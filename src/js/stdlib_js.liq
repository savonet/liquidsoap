%include "error.liq"
%include "null.liq"
%include "ref.liq"
%include "list.liq"
%include "math.liq"
%include "getter.liq"
%include "predicate.liq"

# Implementation for this is in core.
def string.id(id) =
  id
end

%include "string.liq"
%include "utils.liq"
