module type Type = sig
  type t

  val iso8859 : t
  val utf8 : t
  val utf16 : t
  val utf16le : t
  val utf16be : t
  val auto : t
  val convert : t -> t -> string -> string
end

module Make (C : CamomileLibrary.CharEncoding.Interface) : Type
