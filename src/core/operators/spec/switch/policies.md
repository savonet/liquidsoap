# Policies (Part A, as-built)

A policy is a list of (predicate, child) given to `switch`. `fallback`,
`rotate` and `random` are policies written in the script language.

Policy files use the section order of this one.

## 1. Children

All three take a list of sources, each optionally carrying the per-source
methods of [composition.md §3](composition.md#3-per-source-methods) and
`weight`, an integer getter. A missing `weight` is 1.

## 2. State

What the policy keeps between predicate calls.

## 3. Predicates

What each predicate computes and what it changes.

## 4. Events

Which stream events update the state.

## 5. Shape

Which operators the policy builds.

## 6. What a predicate may assume today

Nothing states it. What the switch does
([switch.md §4](switch.md#4-predicates), [§6](switch.md#6-get_source)):

- Predicates run only when a selection is searched, at most once per cycle,
  all of them together, in list order, inside one atomic section.
- A search happens: with no selection; at a track boundary of the selection;
  on every cycle when some child is not track-sensitive; when the selection is
  not ready; and when the switch resumes.
- A search does not imply that a track ended (resuming, non-track-sensitive
  children).
- A track counted by the switch's track callback may not have been heard
  ([frame-assembly.md §6](frame-assembly.md#6-consequences)).
