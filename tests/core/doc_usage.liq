# The usage template of a function: required labeled arguments first, then
# optional ones at their default, then unlabeled ones in order, named after
# their variable unless it is a single letter.

# Sends a message.
def send(~retries=3, ~host, ~on_reply, message, f) =
  ignore(retries + 1)
  ignore(host ^ "")
  on_reply("", 1.)
  ignore(message ^ "")
  ignore(f(1))
end

# Connects to a host.
def connect(~options, ~on_error) =
  ignore(options.port + 1)
  on_error(reason="", 1.)
end
