class weak src =
  let weak = Weak.create 1 in
  let () = Weak.set weak 0 (Some (src :> Source.source)) in
  object (self)
    inherit Source.operator ~name:"weak" []
    method private src = Option.get (Weak.get weak 0)

    initializer
      Clock.unify ~pos:self#pos self#clock self#src#clock;
      self#on_wake_up (fun () -> self#src#wake_up (self :> Clock.source));
      self#on_sleep (fun () -> self#src#sleep (self :> Clock.source))

    method fallible = self#src#fallible
    method private can_generate_frame = self#src#is_ready
    method abort_track = self#src#abort_track
    method remaining = self#src#remaining
    method self_sync = self#src#self_sync
    method seek_source = self#src#seek_source
    method private generate_frame = self#src#get_frame
  end
