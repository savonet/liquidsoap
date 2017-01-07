module Ssl_output =
struct
  include Harbor_ssl.Transport
  let source_name = "output.harbor.ssl"
  let source_description = "Encode and output the stream using the SSL harbor server."
end

module Ssl = Harbor_output.Make(Ssl_output)

include Ssl
