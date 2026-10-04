#!/usr/bin/env python3
"""Negative controls for the known-complexity checks.

Each mutant breaks the clock in one way and names the check that must then
fail. A check only counts once it has been seen to fail here.

Run from the repository root: python3 src/core/clock/test/mutants.py [name]
"""
import subprocess, sys, os

DIR = "src/core/clock"
EXE = "_build/default/src/core/clock/test/conformance.exe"

# (name, group, check that must fail, [(file, old, new), ...])
MUTANTS = [
    ("K1-asks-every-tick", "sync", "K1:", [("pacing.ml",
        "  let changes = List.rev (Atomic.exchange st.changes []) in\n",
        "  let changes = List.rev (Atomic.exchange st.changes []) in\n  List.iter (fun ((s : source), _) -> ignore s#sync_source) (members st);\n")]),
    ("K2-no-unsubscribe", "core", "K2: a detached source is unsubscribed", [("activation.ml",
        'Option.iter (quietly c "unsubscribing from a source") unsubscribe',
        "ignore unsubscribe")]),
    ("K2-keeps-sources", "core", "K2: discarded sources are not kept alive", [("activation.ml",
        "let activate_source st (source : source) =\n",
        "let activate_source st (source : source) =\n  Stdlib.at_exit (fun () -> ignore (Sys.opaque_identity source));\n")]),
    ("K3-entries-pile-up", "core", "K3: the entry count returns", [("life.ml",
        "            set_registrants p sub 0;\n", "")]),
    ("K3-sub-not-stopped", "core", "K3: a sub-clock deregistered by an output", [
        ("life.ml", "            set_registrants p sub 0;\n            stop_clock (get sub) `Parent_stopped",
                          "            set_registrants p sub 0"),
        ("life.ml", "                    stop_clock (get sub) `Parent_stopped))\n              subs;",
                          "                    stop_clock (get sub) `Parent_stopped))\n              (ignore subs; Atomic.get c.subs);")]),
    ("K3-two-entries", "unify", "K3: two unified sub-clocks of one parent are one entry", [("unify.ml",
        "      List.iter merge_subs\n", "      List.iter ignore\n")]),
    ("K4-compare-content", "unify", "K4: deduplicating", [("state.ml",
        "let compare a b = Int.compare (get a).identity (get b).identity",
        "let compare a b =\n  let content c = (Atomic.get c.error_handlers, c.identity) in\n  Stdlib.compare (content (get a)) (content (get b))")]),
    ("K4-sync-by-name", "sync", "K4: two sync sources", [("sync_source.ml",
        "let equal a b = a.identity = b.identity", "let equal a b = a.name = b.name")]),
    ("K5-registry-keeps-clocks", "core", "clocks created and discarded do not accumulate", [("state.ml",
        "  let watch c = Gc.finalise (fun c -> push dead (c.identity, Atomic.get c.id)) c",
        "  let watch c =\n    Stdlib.at_exit (fun () -> ignore (Sys.opaque_identity c));\n    Gc.finalise (fun c -> push dead (c.identity, Atomic.get c.id)) c")]),
    ("K5-pass-not-linear", "core", "K5: a start pass examines each waiting clock once", [("life.ml",
        "  List.length waiting\n", "  List.length waiting * List.length waiting\n")]),
    ("K6-change-not-applied", "sync", "K6: it is applied at the next tick", [("pacing.ml",
        "if Atomic.exchange st.dirty false || changes <> [] then", "if Atomic.exchange st.dirty false then")]),
    ("K9-clock-paces-device", "pacing", "K9: a device-paced stream runs in real time", [("pacing.ml",
        "  stop_check ();\n  if not (measures c st && rest_or_lateness c st) then time_box c st",
        "  stop_check ();\n  Thread_utils.delay st.frame_duration;\n  if not (measures c st && rest_or_lateness c st) then time_box c st")]),
    ("K9-rests-on-device", "pacing", "a clock following a self-paced sync source never rests", [("state.ml",
        "          | Some { pacing = `Self_paced } -> false", "          | Some { pacing = `Self_paced } -> true")]),
    ("K10-ended-ignored", "pacing", "K10: ending the server stops its clocks", [("pacing.ml",
        "  if result = `Ended then begin", "  if false && result = `Ended then begin")]),
    ("K11-failed-plan-leaves-traces", "unify", "K11: a failed unification leaves every clock", [("unify.ml",
        "  else raise (Conflict { left = clock_name ca; right = clock_name cb })",
        "  else begin\n    Atomic.set ca.pending [];\n    Atomic.set cb.pending [];\n    raise (Conflict { left = clock_name ca; right = clock_name cb })\n  end")]),
    ("K11-leftover-in-registry", "unify", "K11: no set holds the clock merged away", [("unify.ml",
        "  Registry.remove x;\n  (y, moved)", "  (y, moved)")]),
    ("K11-memory", "unify", "K11: creating, unifying and discarding clocks uses flat memory", [("unify.ml",
        "  Registry.remove x;\n  (y, moved)", "  Registry.run x;\n  (y, moved)")]),
    ("K11-sources-not-moved", "unify", "K11: the sources of the other clock join", [("unify.ml",
        "    (pending\n    @ List.filter (fun s -> not (List.memq s pending)) (Atomic.get x.pending));",
        "    pending;")]),
    ("K11-unlocked", "unify", "K11:", [("unify.ml",
        "let unify a b =\n  Transition.run (fun () ->", "let unify a b =\n  (fun fn -> fn ()) (fun () ->")]),
    ("K12-no-time-box", "two_workers", "K12: 4 late clocks on 2 workers take turns", [("pacing.ml",
        ">= conf_time_box#get", ">= infinity")]),
    ("K12-unsynced-as-task", "pacing", "K12: with", [("pacing.ml",
        "  else if c.sync_mode = `Unsynced then (`Thread, \"unsynced\")", "")]),
    ("K12-release-yields-to-all", "one_worker", "a late clock is picked again before lower-ranked work", [("pacing.ml",
        "    | Some (`Task, _), `Release -> Duppy.reschedule ~priority:`Clock scheduler",
        "    | Some (`Task, _), `Release -> Duppy.reschedule ~priority:`Threaded scheduler")]),
    ("K13-stop-does-not-interrupt", "shutdown", "K13: a resting clock stops", [("life.ml",
        "        Option.iter interrupt (Atomic.get c.streaming);\n", "")]),
    ("K13-no-stop-check", "shutdown", "ticking after the global stop fails with the stop signal", [("state.ml",
        "let stop_check () = if Atomic.get global_stop then raise Stop_signal", "let stop_check () = ()")]),
    ("K15-no-controller", "core", "K15: a passive clock cannot be created without a controller", [("clock.ml",
        '        invalid_arg "Clock.create: a passive clock needs a parent or an owner"', "        ()")]),
    ("K15-owners-ignored", "unify", "K15: two exclusive child clocks stay two", [("unify.ml",
        "  if ca.sync_mode = `Passive && cb.sync_mode = `Passive && ca.owner <> cb.owner", "  if false && ca.owner <> cb.owner")]),
    ("K17-output-let-go", "core", "K17: an output stays awake", [("activation.ml",
        "        let sleep = source#wake_up () in\n", "        let sleep = source#wake_up () in\n        sleep ();\n")]),
    ("K17-not-put-to-sleep", "core", "K17: winding down puts it to sleep", [("life.ml",
        '              (fun o -> quietly c "putting an output to sleep" o.sleep)', "              (fun _ -> ())")]),
    ("K18-no-nesting-check", "unify", "K18:", [("unify.ml",
        "      (try check_nesting merges", "      (try ignore merges")]),
    ("wait-holds-worker", "one_worker", "a tick waiting, on one worker", [("wait.ml",
        "      when (match Atomic.get st.animator with", "      when false && (match Atomic.get st.animator with")]),
]

def sh(cmd, timeout=300):
    return subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)

def run(name, group, expected, edits):
    saved = {}
    try:
        for file, old, new in edits:
            path = os.path.join(DIR, file)
            text = saved.setdefault(path, open(path).read())
            current = open(path).read()
            if old not in current:
                return f"STALE  {name}: pattern not found in {file}"
            open(path, "w").write(current.replace(old, new, 1))
        build = sh(f"dune build ./{DIR}/test/conformance.exe 2>&1")
        if build.returncode != 0:
            return f"BROKEN {name}: mutant does not build\n{build.stdout[:400]}"
        try:
            result = sh(f"timeout 120 ./{EXE} {group} 2>&1", timeout=150)
        except subprocess.TimeoutExpired:
            return f"KILLED {name}: group {group} hung"
        failed = [l for l in result.stdout.splitlines() if l.startswith("FAIL")]
        if any(expected in l for l in failed):
            return f"KILLED {name}: {group} failed '{expected}'"
        if result.returncode != 0:
            return f"CRASH  {name}: {group} exited {result.returncode} without the named failure: {failed[:2]} {result.stdout[-200:]!r}"
        return f"ALIVE  {name}: {group} passed"
    finally:
        for path, text in saved.items():
            open(path, "w").write(text)

if __name__ == "__main__":
    wanted = sys.argv[1:]
    results = [run(*m) for m in MUTANTS if not wanted or m[0] in wanted]
    print("\n".join(results))
    killed = sum(r.startswith("KILLED") for r in results)
    print(f"{killed}/{len(results)} mutants killed")
    sh(f"dune build ./{DIR}/test/conformance.exe")
    sys.exit(0 if results and killed == len(results) else 1)
