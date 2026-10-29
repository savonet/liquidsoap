external of_closure : (unit -> unit) -> string = "liquidsoap_code_digest"

let marker () = ()
let get () = of_closure marker
