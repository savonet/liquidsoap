# Policies (Part A)

A policy is a list of (predicate, child) given to [`switch`](switch.md),
with the state its predicates read. `fallback`, `rotate` and `random` are
policies written in the script language.

Policy files use the section order of this one, up to §5.

## 1. Children

All three take a list of sources, each optionally carrying the per-source
methods of [composition.md §3](composition.md#3-per-source-methods) and
`weight`, an integer getter. A missing `weight` is 1.

## 2. State

What the policy keeps between decisions.

## 3. Predicates

What each predicate computes, from the state and from probes.

## 4. Events

Which stream events change the state.

## 5. Shape

What the policy guarantees about the operators it builds. Construction is
free.

## 6. Predicate contract

What a predicate MUST do:

- Change nothing when it is read: no counter, no pick, no flag. Its result
  depends only on state that events change, on getters, and on probes
  ([source-contract.md §2](source-contract.md#2-probes)).
- Hold for at most one child at a time when the policy means to designate
  one child.

What a predicate MAY assume of the switch:

- It is read only to make a decision, inside one atomic section with the
  other predicates of that decision
  ([switch.md §3](switch.md#3-predicates)).
- It MAY be read any number of times for one track, and not at all for
  another. A read does not mean that a track ended, nor that the child will
  be played.
- No predicate is read while a track-sensitive selection plays and no child
  could take over.

State that must advance when a child is played advances on an event, never
in a predicate:

- the switch's **track event** fires when a track start is delivered to a
  request ([switch.md §5](switch.md#5-handoff)). It means the track is being
  played. `selected()` then names the child it comes from;
- a child's `on_select` runs when that child is committed.

A predicate that consumes what it reads (true once, then false) is not
supported: a decision can be made and not committed. Such a condition is
reset from `on_select`.

## 7. Fallibility

A policy over children of which one is infallible MUST give an infallible
operator when it guarantees that an available child is always designated.
`fallback`, `rotate` and `random` do.
