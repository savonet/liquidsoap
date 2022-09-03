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

module Make (C : CamomileLibrary.CharEncoding.Interface) = struct
  type t = C.t

  let iso8859 = C.of_name "ISO-8859-1"
  let utf8 = C.utf8
  let utf16 = C.utf16
  let utf16le = C.utf16le
  let utf16be = C.utf16be
  let auto = C.automatic "auto" [iso8859; utf8; utf16] utf8
  let convert in_enc out_enc s = C.recode_string ~in_enc ~out_enc s
end
