# The help of a function lists the fields of a record argument once one of
# them is documented with `@field`.

# Connects to a server.
# @param config The connection settings.
# @field config.port TCP port.
# @field config.host Host name, \
#   resolved at connection.
# @field ~retry.count Number of attempts.
def connect(~retry, config) =
  ignore(retry?.count ?? 1)
  ignore(config.port + 1)
  ignore(config?.host ?? "")
  ignore(config.name ^ "")
end
