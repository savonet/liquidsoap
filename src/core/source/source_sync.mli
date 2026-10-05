(** What a source declares about pacing: whether its answer can change at run
    time, and the sync source it currently reports. See spec/pacing.md §3. *)
type t = [ `Static | `Dynamic ] * Clock.Sync_source.t option

(** [`Dynamic] if any of the sources is. Computed once, on first call. *)
val type_of_sources :
  < self_sync : t ; .. > list -> unit -> [ `Static | `Dynamic ]

(** The answer of an operator that reads all of these sources: the single sync
    source among those that are ready. Raises [Clock.Sync_error] when they
    report two. *)
val of_sources :
  < id : string
  ; stack : Pos.t list
  ; is_ready : bool
  ; self_sync : t
  ; sync_source : Clock.Sync_source.t option
  ; .. >
  list ->
  ?source:< id : string ; .. > ->
  unit ->
  t

val same : Clock.Sync_source.t option -> Clock.Sync_source.t option -> bool
