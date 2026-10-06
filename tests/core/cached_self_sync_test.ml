(* [cached_self_sync] computes a source's answer once per streaming cycle. *)

class counting =
  object (self)
    inherit Debug_sources.fail "counting"
    val computed = Atomic.make 0
    method computed = Atomic.get computed
    method! can_generate_frame = true
    method! generate_frame = self#empty_frame

    method! private self_sync =
      Atomic.incr computed;
      (`Static, None)
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let source = new counting in
  let clock = Clock.create ~sync:`Passive () in
  Clock.start ~force:true clock;
  let output =
    new Output.dummy
      ~clock ~autostart:true ~infallible:false ~register_telnet:false
      (Lang.source (source :> Source.source))
  in
  output#content_type_computation_allowed;
  let activation = output#wake_up (output :> Clock.source) in
  Clock.tick clock;
  assert source#is_ready;
  ignore source#cached_self_sync;
  let in_cycle = source#computed in
  ignore source#cached_self_sync;
  ignore source#cached_self_sync;
  assert (source#computed = in_cycle);
  Clock.tick clock;
  assert source#is_ready;
  ignore source#cached_self_sync;
  assert (source#computed > in_cycle);
  output#sleep activation
