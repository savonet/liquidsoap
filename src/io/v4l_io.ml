external caps :
  Unix.file_descr -> string * int * int * int * int * int * int
  = "caml_v4l_caps"
external init : Unix.file_descr -> unit = "caml_v4l_init"
external get_dims : Unix.file_descr -> int * int = "caml_v4l_get_dims"
external capture : Unix.file_descr -> int -> int -> string = "caml_v4l_capture"

let every = 5

class input ~kind dev =
object (self)
  inherit Source.active_source kind

  val mutable fd = None

  method stype = Source.Infallible
  method remaining = -1
  method is_ready = true

  method abort_track = ()

  method output = if AFrame.is_partial memo then self#get_frame memo

  val mutable width = 0
  val mutable height = 0

  method output_get_ready =
    fd <- Some (Unix.openfile dev [Unix.O_RDWR] 0);
    let fd = Utils.get_some fd in
    let _, _, _, _, _, _, _ = caps fd in
      init fd;
      let w, h = get_dims fd in
        width <- w;
        height <- h

  method output_reset = ()
  method is_active = true

  val mutable image = RGB.create 0 0
  val mutable count = every

  method get_frame frame =
    assert (0 = AFrame.position frame);
    let fd = Utils.get_some fd in
    let buf = VFrame.content_of_type ~channels:1 frame 0 in
    let buf = buf.(0) in
    let img =
      (*
      let buflen = width * height * 3 in
      let buf = String.make buflen '\000' in
        ignore (Unix.read fd buf 0 buflen);
        buf
       *)
      if count = every then
        (
          count <- 0;
          RGB.of_linear_rgb (capture fd width height) width
        )
      else
        (
          count <- count + 1;
          image
        )
    in
      image <- img;
      for i = 0 to VFrame.size frame - 1 do
        RGB.proportional_scale buf.(i) img
      done;
      AFrame.add_break frame (AFrame.size ())
end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained
         { Frame. audio = Lang.Any_fixed 0 ; video = Lang.Fixed 1 ; midi = Lang.Fixed 0 })
  in
  Lang.add_operator "input.v4l"
    [
      "device", Lang.string_t, Some (Lang.string "/dev/video0"),
      Some "V4L device to use.";
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Stream from a V4L (= video 4 linux) input device, such as a webcam."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let device = e Lang.to_string "device" in
         ((new input ~kind device):>Source.source)
    )
