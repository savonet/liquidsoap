module Ssl_input =
struct
  include Harbor_ssl
  let source_name = "input.harbor.ssl"
  let source_description = "Retrieves the given https stream from the harbor."
end

module Ssl = Harbor_input.Make(Ssl_input)

include Ssl
