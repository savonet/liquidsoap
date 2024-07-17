
float = float_of_int

def mksafe(s)
  fallback(track_sensitive=false,[s,blank()])
end

output.prefered=output.dummy
%ifdef output.oss
  output.prefered=output.oss
%endif
%ifdef output.alsa
  output.prefered=output.alsa
%endif
%ifdef output.ao
  output.prefered=output.ao
%endif

in = fun () -> blank()
%ifdef input.oss
  in = fun () -> input.oss()
%endif
%ifdef input.alsa
  in = fun () -> input.alsa()
%endif

def out(s)
  output.prefered(mksafe(s))
end

def crossfade(~start_next,~fade_in,~fade_out,s)
  s = fade.in(duration=fade_in,s)
  s = fade.out(duration=fade_out,s)
  fader = fun (a,b) -> add(normalize=false,[b,a])
  cross(duration=start_next,fader,s)
end

def say_metadata
  p = 'say:$(if $(artist),"It was $(artist)$(if $(title),\", $(title)\").")'
  fun (s,~pattern=p) ->
    append(s,fun (m) -> request.queue(queue=[request(pattern % m)],
                                      interactive=false))
end

def dolebrai
  input.http(id="dolebrai","http://dolebrai.net:8000/dolebrai.ogg")
end

%ifdef soundtouch
def helium(s)
  soundtouch(pitch=1.5,s)
end
%endif

# Remove low frequencies often produced by microphones
def mic_filter(s)
  filter(freq=200.,q=1.,mode="high",s)
end

def smart_crossfade (~start_next=3.,~fade_in=3.,~fade_out=3.,
                     ~width=1.,~conservative=false,s)
  mf_high = 20
  mf_medium = 32
  mf_margin = 4
  fade.out = fade.out(duration=fade_out)
  fade.in = fade.in(duration=fade_in)
  add = fun (a,b) -> add(normalize=false,[b,a])

  def transition0 (a, b)
    log("No transition, just sequencing.")
    sequence([a, b])
  end
  def condition0 (a, b)
    (a + mf_high >= 0 and b + mf_high >= 0)
    or (a + mf_medium >= 0 and b + mf_medium >= 0 and abs(a - b) <= mf_margin)
  end

  def transition1 (a, b)
    log("Using transition 1: crossed.")
    add(fade.out(a),fade.in(b))
  end
  def condition1 (a, b)
    a + mf_medium <= 0 and b + mf_medium <= 0 and abs(a - b) <= mf_margin
  end

  def transition2 (a, b)
    log("Using transition 2: crossed, fade-out.")
    add(fade.out(a),b)
  end
  def condition2 (a, b)
    b >= a + mf_margin and a <= mf_high
  end

  def transition3 (a, b)
    log("Using transition 3: crossed, fade-in.")
    add(a,fade.in(b))
  end
  def condition3 (a, b)
    a >= b + mf_margin and b <= mf_high
  end

  def transition4 (a, b)
    log("Using transition 4: crossed, fade-in, fade-out.")
    add(fade.out(a),fade.in(b))
  end
  def condition4 (a, b)
    true
  end

  smart_cross([(condition0,transition0),
               (condition1,transition1),
               (condition2,transition2),
               (condition3,transition3),
               (condition4,transition4)],
               width=width,
               duration=start_next,conservative=conservative,
               s)
end
