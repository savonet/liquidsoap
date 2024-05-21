type state

module Ppx_hash_lib : sig
  module Std : sig
    module Hash :
      Ppx_hash_lib.Std.Hash.Full
        with type state = state
         and type seed = string
         and type hash_value = string
  end
end

include module type of Ppx_hash_lib.Std.Hash with type state := state
include module type of Ppx_hash_lib.Std.Hash.Builtin
