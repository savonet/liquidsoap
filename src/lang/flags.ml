type flags = int
type flag = int

let empty = 0
let octal_int = 0b1
let hex_int = 0b10
let checked_value = 0b100
let itered_value = 0b1000
let has flags flag = flags land flag <> 0
let add flags flag = flags lor flag
