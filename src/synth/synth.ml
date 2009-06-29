let pi = 3.1416

let freq_of_note n = 440. *. (2. ** ((float n -. 69.) /. 12.))

(* Global state and note state. *)
class virtual ['gs,'ns] synth =
object (self)
  val mutable state = None

  method state =
    match state with
      | Some s -> s
      | None -> assert false

  val mutable notes = []

  method virtual state_init : 'gs

  method virtual note_init : int -> float -> 'ns

  method init =
    state <- Some (self#state_init)

  method note_on n v =
    notes <- (n, ref (self#note_init n v))::notes

  method note_off n (v:float) =
    notes <- List.filter (fun (m, _) -> m <> n) notes

  method synth_note_mono (gs:'gs) (ns:'ns) (freq:float) (buf:float array) (ofs:int) (len:int) = gs, ns

  method synth_note gs ns freq buf ofs len =
    let s = ref None in
    let chans = Array.length buf in
      for c = 0 to chans - 1 do
        s := Some (self#synth_note_mono gs ns freq buf.(c) ofs len)
      done;
      match !s with
        | Some s -> s
        | None -> gs, ns

  method synth freq buf ofs len =
    let gs = ref self#state in
      List.iter
        (fun (_, ns) ->
           let gs', ns' = self#synth_note self#state !ns freq buf ofs len in
             gs := gs';
             ns := ns'
        ) notes;
      state <- Some !gs
end

type sine_gs = unit
type sine_ns =
    {
      sine_phase : float;
      sine_freq : float;
      sine_ampl : float;
    }

class sine =
object (self)
  inherit [sine_gs, sine_ns] synth

  method state_init = ()

  method note_init n v = { sine_phase = 0.; sine_freq = freq_of_note n; sine_ampl = v }

  method synth_note_mono gs ns freq buf ofs len =
    let phase i = ns.sine_phase +. float i /. freq *. ns.sine_freq *. 2. *. pi in
      for i = ofs to ofs + len - 1 do
        buf.(i) <- ns.sine_ampl *. sin (phase i)
      done;
      gs, { ns with sine_phase = phase len}
end
