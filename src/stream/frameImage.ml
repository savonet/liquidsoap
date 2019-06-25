module I = Image.RGBA32
module Generic = Image.Generic

type t = I.t

let create width height = I.create width height

let of_RGB24_string s width = failwith "Not implemented: of_RGB24_string"

let of_I420_string s width = failwith "Not implemented: of_I420_string"

let to_int_image img = failwith "Not implemented: to_int_image"

let to_generic img dst =
  Generic.convert (Generic.of_RGBA32 img) dst

let width img = I.width img

let height img = I.height img

let dimensions img =
  width img, height img

let copy = I.copy

let blit ?(blank=true) ?(x=0) ?(y=0) ?w ?h src dst =
  if x = 0 && y = 0 && w = None && h = None then
    I.blit_all src dst
  else
    I.blit ~blank ~x ~y ?w ?h src dst

let size img =
  Bigarray.Array1.size_in_bytes (I.data img)

let blank img = failwith "Not implemented: blank"

let randomize img = failwith "Not implemented: randomize"

let get_pixel img x y = failwith "Not implemented: get_pixel"

let set_pixel img x y (r,g,b,a) = failwith "Not implemented: set_pixel"

let add src ?(x=0) ?(y=0) dst = failwith "Not implemented: add"

module Effect = struct
  let greyscale img = failwith "Not implemented: greyscale"

  let sepia img = failwith "Not implemented: sepia"

  let invert img = failwith "Not implemented: invert"

  let lomo img = failwith "Not implemented: lomo"

  let translate img x y = failwith "Not implemented: translate"

  module Alpha = struct
    let scale img x = failwith "Not implemented: Alpha.scale"
  end
end
