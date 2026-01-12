module Specs = struct
  let description = "md5"

  type state = string

  let fold_int s i = Digest.string (s ^ string_of_int i)
  let fold_int64 s i = Digest.string (s ^ Int64.to_string i)
  let fold_float s f = Digest.string (s ^ string_of_float f)
  let fold_string s v = Digest.string (s ^ v)

  type seed = string

  let alloc () = ""
  let reset ?(seed = "") _ = seed

  type hash_value = string

  let get_hash_value = Digest.to_hex

  module For_tests = struct
    let compare_state = Stdlib.compare
    let state_to_string s = s
  end
end

module Ppx_hash_lib = struct
  module Std = struct
    module Hash = Ppx_hash_lib.Std.Hash.F (Specs)
  end
end

include Ppx_hash_lib.Std.Hash
include Ppx_hash_lib.Std.Hash.Builtin
