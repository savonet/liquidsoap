module Image = FrameImage

type t = Image.t array

let make len width height =
  Array.init len (fun _ -> Image.create width height)

let single img = [|img|]

let blit sbuf sofs dbuf dofs len =
  for i = 0 to len - 1 do
    Image.blit sbuf.(sofs + i) dbuf.(dofs + i)
  done

let copy vid =
  Array.map Image.copy vid

let length vid =
  Array.length vid

let size vid =
  let n = ref 0 in
  for i = 0 to Array.length vid - 1 do
    n := !n + Image.size vid.(i)
  done;
  !n

let get vid i = vid.(i)

let iter f vid off len =
  for i = off to off + len - 1 do
    f vid.(i)
  done

let blank vid off len =
  iter Image.blank vid off len
