
(** Here is the definition of what's a [source], a streamer, a radio.
  * This definition is probably the most important in liquidsoap. *)

(** The liveness type of a source indicates whether or not it can
  * fail to broadcast.
  * A Infallible source never fails is always ready.
  *
  * In order to infer liveness information an operator, we must now
  * if it is a "And" operator, or an "Or" one.
  * A "And" operator behaves as a Infallible source if and only if
  * all of its sources are Infallible.
  * A "Or" operator is Infallible if at least one of its sources is Infallible.
  * Typically, an operator that switches between 2 sources is an "Or" one,
  * and an operator that mixes 2 sources together is an "And" one. *)
type source_t = Fallible | Infallible

(** The [source] use is to send music frames through the [get] method. *)
class virtual source :
object

  (** Identifier of the source. *)
  val mutable id : string
  method id : string

  (** Give a special name to a source, typically its name in the program. *)
  method set_name : string -> unit

  (** Number of frames left in the current track. Defaults to -1=infinity. *)
  method remaining : int

  (** The operator says to the source that he will ask it frames. *)
  method get_ready : operator -> unit

  (** Called when the source must be ready and had no active operator,
    * means that the source has to initialize. *)
  method virtual wake_up : unit

  (** [get buf] asks the source to fill the buffer [buf] if possible. 
    * We say that [get] fails when nothing is added to the buffer.
    * The [get] call is partial when the buffer is not completely filled.
    * [get] should never be called with a full buffer. *)
  method virtual get : Mixer.Buffer.t -> unit

  (** [is_ready] tells you if [get] would succeed. *)
  method virtual is_ready : bool

  (** Tells the source to finish the reading of current track. *)
  method virtual abort_track : unit

  (** Opposite of [get_ready] : the operator no longer needs the source. *)
  method leave : operator -> unit
  method virtual sleep : unit

  (** {1 Liveness type}
    *
    * [stype] is the liveness type, telling whether a scheduler is
    * fallible or not, i.e. [get] will never fail.
    * It is defined in the derived classes. *)

  method stype : source_t

  (** {1 Safety type}
    *
    * The goal is to avoid the situation where two operators are asking frames
    * to the same source at the same time.
    * We compute [activation_intervals]
    * which must be a set that contains every instant where 
    * a source could be broadcasted.
    * The root's activation interval is "always" and each operator
    * tells its sources when it is possible that it broadcasts the source
    * so that the source's activation interval can be computed. 
    * A scheduler S is safe if it has no two fathers A and B such
    * that A told [S#add_activation_interval I] and B told
    * [S#add_activation_interval J] where I and J intersect. 
    *
    * Prior to performing safety-typing, we compute the number of 
    * fathers of a source and the source id (which is a path in the DAG
    * from the root to the source) by the same time in set_id. 
    *
    * All of these methods should not be redefined.
    *
    * Good news : the information that will be computed will also give
    * some liveness info. It will be possible in some cases to detect that
    * some branches of the DAG will never be reached. *)

  val mutable activation_intervals : Scheduling.cexpr
  method add_activation_interval : Scheduling.cexpr -> bool
  val mutable number_of_fathers : int
  method set_id : string -> unit

  (** Create a request with a "source" metadata. *)
  method create_request : string -> Request.t option

end

(** An [operator] is just a [source] which takes [source]s as arguments.
  * The point of this definition is to make the type inference automatic
  * if you simply define the list of your childs. *)
and virtual operator : source list ->
object
  inherit source

  (** Propagates the safety-typing check,
    * returns true if everything is OK. *)
  method propagate_typing : bool

end
