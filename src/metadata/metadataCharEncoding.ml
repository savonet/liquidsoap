module C = CamomileLibraryDefault.Camomile.CharEncoding

let iso8859 = C.of_name "ISO-8859-1"
let utf8 = C.utf8
let utf16 = C.utf16
let convert in_enc out_enc s = C.recode_string ~in_enc ~out_enc s
