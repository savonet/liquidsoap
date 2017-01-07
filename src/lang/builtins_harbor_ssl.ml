module Ssl_harbor =
struct
  include Harbor_ssl
  let name = "https"
end

module Ssl = Builtins_harbor.Make(Ssl_harbor)

include Ssl
