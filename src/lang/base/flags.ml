type flags = int
type flag = int

let empty = 0
let octal_int = 1
let hex_int = 1 lsl 1
let checked_value = 1 lsl 2
let itered_value = 1 lsl 3
let binary = 1 lsl 4
let has flags flag = flags land flag <> 0
let add flags flag = flags lor flag
let remove flags flag = flags land lnot flag
