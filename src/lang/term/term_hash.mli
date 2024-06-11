type state

module Ppx_hash_lib : sig
  module Std : sig
    module Hash :
      Ppx_hash_lib.Std.Hash.Full
        with type state = state
         and type seed = string
         and type hash_value = string
  end

  type 'a hash_fold = Std.Hash.state -> 'a -> Std.Hash.state

  module Hashable : sig
    module type S = sig
      type t

      val hash_fold_t : t hash_fold
      val hash : t -> Std.Hash.hash_value
    end

    module type S1 = sig
      type 'a t

      val hash_fold_t : 'a hash_fold -> 'a t hash_fold
    end

    module type S2 = sig
      type ('a, 'b) t

      val hash_fold_t : 'a hash_fold -> 'b hash_fold -> ('a, 'b) t hash_fold
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val hash_fold_t :
        'a hash_fold -> 'b hash_fold -> 'c hash_fold -> ('a, 'b, 'c) t hash_fold
    end
  end
end

include module type of Ppx_hash_lib.Std.Hash with type state := state
include module type of Ppx_hash_lib.Std.Hash.Builtin
