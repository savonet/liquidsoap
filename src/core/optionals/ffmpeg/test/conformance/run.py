#!/usr/bin/env python3
"""Conformance checks of the FFmpeg layer that need media and an external
oracle (spec/tests.md): each check runs a Liquidsoap script, lists the
packets of its output with ffprobe, and asserts on them.

Usage: run.py [--mutations] [check ...]

With --mutations, every check is also run against each of its named
mutations (tests.md §1.3): a minimal source edit applied, built and
reverted by this script. A check passes its negative control only when the
mutated build fails it on an assertion.
"""

import json
import os
import subprocess
import sys
import tempfile
from collections import defaultdict
from fractions import Fraction

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, *[".."] * 6))
FIXTURES = os.environ.get("LIQ_FFMPEG_FIXTURES", "/var/tmp/liquidsoap-ffmpeg-fixtures")
LIQUIDSOAP = os.path.join(ROOT, "liquidsoap")
PRODUCER = os.path.join(ROOT, "_build/default/src/core/optionals/ffmpeg/test/copy_producer.exe")


class CheckFailed(Exception):
    pass


class Counter:
    def __init__(self):
        self.assertions = 0

    def check(self, name, cond):
        self.assertions += 1
        if not cond:
            raise CheckFailed(name)


def fixtures():
    os.makedirs(FIXTURES, exist_ok=True)
    subprocess.run([os.path.join(HERE, "fixtures.sh"), FIXTURES], check=True)


def fixture(name):
    return os.path.join(FIXTURES, name)


def run_script(script, timeout=300):
    with tempfile.NamedTemporaryFile("w", suffix=".liq", dir=FIXTURES, delete=False) as f:
        f.write(script)
        path = f.name
    try:
        p = subprocess.run([LIQUIDSOAP, path], cwd=ROOT, capture_output=True,
                           text=True, timeout=timeout)
    finally:
        os.unlink(path)
    if p.returncode != 0:
        raise CheckFailed("liquidsoap exited with %d: %s" % (p.returncode, p.stderr[-2000:]))
    return p.stdout + p.stderr


def produce(scenario, fmt, out, sources):
    """The synthetic copy producer (tests.md §3) into a copy encoder."""
    p = subprocess.run([PRODUCER, scenario, fmt, out] + [fixture(f) for f in sources],
                       capture_output=True, text=True, timeout=300)
    if p.returncode != 0:
        raise CheckFailed("copy producer exited with %d: %s" % (p.returncode, p.stderr[-2000:]))


def packets(path):
    """Per stream type: the packets in file order, times as Fractions of a
    second, and the stream time base."""
    out = subprocess.run(
        ["ffprobe", "-v", "error", "-show_packets", "-show_streams", "-of", "json", path],
        capture_output=True, text=True, check=True).stdout
    data = json.loads(out)
    tb = {s["index"]: Fraction(s["time_base"]) for s in data["streams"]}
    kind = {s["index"]: s["codec_type"] for s in data["streams"]}
    streams = defaultdict(list)
    for p in data.get("packets", []):
        i = p["stream_index"]

        def t(k):
            v = p.get(k)
            return None if v is None else int(v) * tb[i]

        streams[kind[i]].append({
            "dts": t("dts"), "pts": t("pts"), "duration": t("duration"),
            "key": "K" in p["flags"], "tb": tb[i], "size": int(p["size"])})
    return streams


def assert_monotonic(c, name, pkts):
    last = None
    for p in pkts:
        if p["dts"] is None:
            continue
        c.check("%s: DTS strictly increasing" % name, last is None or p["dts"] > last)
        c.check("%s: PTS >= DTS" % name, p["pts"] is None or p["pts"] >= p["dts"])
        last = p["dts"]


def copy_script(sources, tracks, out, fmt):
    files = ", ".join('once(single("%s"))' % fixture(s) for s in sources)
    return """
s = sequence([%s])
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="%s", %s), "%s", s)
o.on_stop(synchronous=true, shutdown)
""" % (files, fmt, tracks, out)


# known-complexity.md §1: rebasing a sequence of copied sources.
def kc1_assertions(c, name, pkts, sources):
    c.check("%s: packet count" % name, len(pkts) == sum(len(s) for s in sources))
    assert_monotonic(c, name, pkts)
    tolerance = pkts[0]["tb"] + max(s[0]["tb"] for s in sources)
    i = 0
    previous_end = None
    for s in sources:
        out_segment = pkts[i:i + len(s)]
        for o, p in zip(out_segment, s):
            c.check("%s: PTS - DTS kept" % name,
                    abs((o["pts"] - o["dts"]) - (p["pts"] - p["dts"])) <= tolerance)
        if previous_end is not None:
            c.check("%s: join after previous end" % name,
                    out_segment[0]["dts"] >= previous_end - tolerance)
        previous_end = out_segment[-1]["dts"] + s[-1]["duration"]
        i += len(s)
    span = pkts[-1]["dts"] + sources[-1][-1]["duration"] - pkts[0]["dts"]
    expected = sum(s[-1]["dts"] + s[-1]["duration"] - s[0]["dts"] for s in sources)
    c.check("%s: total duration" % name, abs(span - expected) <= Fraction(1, 30))


def kc1(c, tmp):
    inputs = ["bf1.mp4", "bf2.mp4", "bf3.mp4"]
    sources = [packets(fixture(f))["video"] for f in inputs]
    for fmt, ext in [("mp4", "mp4"), ("mpegts", "ts")]:
        out = os.path.join(tmp, "kc1." + ext)
        run_script(copy_script(inputs, "%video.copy", out, fmt))
        kc1_assertions(c, "kc1 " + fmt, packets(out)["video"], sources)
    # Positions that fall short of timestamps, and packets with no duration.
    for scenario in ["short", "nodur"]:
        out = os.path.join(tmp, "kc1_%s.ts" % scenario)
        produce(scenario, "mpegts", out, inputs)
        kc1_assertions(c, "kc1 " + scenario, packets(out)["video"], sources)


def steps(pkts):
    return [b["dts"] - a["dts"] for a, b in zip(pkts, pkts[1:])]


# known-complexity.md §2: one offset per source, aligned on the longest.
def kc2(c, tmp):
    joins = 10
    src = packets(fixture("avoff.mp4"))
    results = []
    for order in ["%audio.copy, %video.copy", "%video.copy, %audio.copy"]:
        out = os.path.join(tmp, "kc2.ts")
        run_script(copy_script(["avoff.mp4"] * (joins + 1), order, out, "mpegts"))
        pkts = packets(out)
        results.append(pkts)
        for kind in ["audio", "video"]:
            c.check("kc2: %s count" % kind, len(pkts[kind]) == len(src[kind]) * (joins + 1))
            assert_monotonic(c, "kc2 " + kind, pkts[kind])
        n_a, n_v = len(src["audio"]), len(src["video"])
        last_a = pkts["audio"][joins * n_a:]
        last_v = pkts["video"][joins * n_v:]
        delta = last_a[0]["pts"] - last_v[0]["pts"]
        expected = src["audio"][0]["pts"] - src["video"][0]["pts"]
        c.check("kc2: A/V start relation kept after %d joins" % joins,
                abs(delta - expected) <= Fraction(1, 30))
        for kind, n in [("audio", n_a), ("video", n_v)]:
            out_tb = pkts[kind][0]["tb"]
            for j in range(joins + 1):
                segment = pkts[kind][j * n:(j + 1) * n]
                for a, b in zip(steps(segment), steps(src[kind])):
                    c.check("kc2: %s steps kept within a source" % kind,
                            abs(a - b) <= out_tb + src[kind][0]["tb"])
    for kind in ["audio", "video"]:
        same = [(p["dts"], p["pts"]) for p in results[0][kind]] == \
               [(p["dts"], p["pts"]) for p in results[1][kind]]
        c.check("kc2: %s identical in both declaration orders" % kind, same)
    # Producer positions far behind timestamps: each source must be anchored
    # after the latest end of the previous one, with one offset.
    out = os.path.join(tmp, "kc2_compressed.ts")
    produce("compressed", "mpegts", out, ["avoff.mp4"] * (joins + 1))
    pkts = packets(out)
    n_a, n_v = len(src["audio"]), len(src["video"])
    for kind in ["audio", "video"]:
        assert_monotonic(c, "kc2 compressed " + kind, pkts[kind])
        c.check("kc2 compressed: %s count" % kind,
                len(pkts[kind]) == len(src[kind]) * (joins + 1))
    for j in range(1, joins + 1):
        new_a, new_v = pkts["audio"][j * n_a], pkts["video"][j * n_v]
        ends = [pkts[k][j * n - 1]["dts"] + src[k][-1]["duration"]
                for k, n in [("audio", n_a), ("video", n_v)]]
        c.check("kc2 compressed: no first packet before the latest previous end",
                min(new_a["dts"], new_v["dts"]) >= max(ends) - Fraction(1, 1000))
        c.check("kc2 compressed: A/V relation kept at each join",
                abs((new_a["pts"] - new_v["pts"]) - expected) <= Fraction(1, 100))
    # A join whose previous source's audio ends after its video: the offset
    # must account for the latest end and keep the new source's own A/V
    # relation, whatever the declaration order.
    first = packets(fixture("gop.mp4"))
    second = packets(fixture("midgop.mp4"))
    key = next(p for p in second["video"] if p["key"])
    first_audio = next(p for p in second["audio"] if p["pts"] >= key["pts"])
    relation = first_audio["pts"] - key["pts"]
    outputs = []
    for order in ["%audio.copy, %video.copy", "%video.copy, %audio.copy"]:
        out = os.path.join(tmp, "kc2b.ts")
        run_script(copy_script(["gop.mp4", "midgop.mp4"], order, out, "mpegts"))
        pkts = packets(out)
        outputs.append(pkts)
        n_a, n_v = len(first["audio"]), len(first["video"])
        ends = [pkts[k][n - 1]["dts"] + pkts[k][n - 1]["duration"]
                for k, n in [("audio", n_a), ("video", n_v)]]
        new_a, new_v = pkts["audio"][n_a], pkts["video"][n_v]
        c.check("kc2: no first packet before the latest previous end",
                min(new_a["dts"], new_v["dts"]) >= max(ends) - Fraction(1, 1000))
        c.check("kc2: new source A/V relation kept",
                abs((new_a["pts"] - new_v["pts"]) - relation) <= Fraction(1, 30))
    for kind in ["audio", "video"]:
        c.check("kc2: %s identical in both orders at a mid-GOP join" % kind,
                [(p["dts"], p["pts"]) for p in outputs[0][kind]]
                == [(p["dts"], p["pts"]) for p in outputs[1][kind]])


# known-complexity.md §3: switching copied streams on keyframes.
def kc3(c, tmp):
    first = packets(fixture("gop.mp4"))
    second = packets(fixture("midgop.mp4"))
    out = os.path.join(tmp, "kc3.ts")
    run_script(copy_script(["gop.mp4", "midgop.mp4"], "%audio.copy, %video.copy", out, "mpegts"))
    pkts = packets(out)
    for kind in ["audio", "video"]:
        assert_monotonic(c, "kc3 " + kind, pkts[kind])
    n_v, n_a = len(first["video"]), len(first["audio"])
    new_v = pkts["video"][n_v:]
    new_a = pkts["audio"][n_a:]
    c.check("kc3: new source has video", len(new_v) > 0)
    c.check("kc3: new source has audio", len(new_a) > 0)
    c.check("kc3: first video packet of the new source is a keyframe", new_v[0]["key"])
    dropped = next(i for i, p in enumerate(second["video"]) if p["key"])
    c.check("kc3: video packets before the keyframe dropped",
            len(new_v) == len(second["video"]) - dropped)
    # The new source starts at the end of the output: the latest end of the
    # previous source's streams (timestamps.md §6.2).
    frame = Fraction(1, 30)
    ends = [pkts[k][n - 1]["dts"] + pkts[k][n - 1]["duration"]
            for k, n in [("video", n_v), ("audio", n_a)]]
    start = min(new_v[0]["dts"], new_a[0]["dts"])
    c.check("kc3: no gap at a whole-output switch",
            0 <= start - max(ends) <= frame / 4)
    c.check("kc3: audio of the new source starts at its keyframe",
            new_a[0]["pts"] >= new_v[0]["pts"] - Fraction(1024, 44100))
    out = os.path.join(tmp, "kc3a.ts")
    run_script(copy_script(["gop.mp4", "midgop.mp4"], "%audio.copy", out, "mpegts"))
    audio = packets(out)["audio"]
    c.check("kc3: audio-only copy discards nothing",
            len(audio) == len(first["audio"]) + len(second["audio"]))

    # Audio from one source, video from another: no shared readiness.
    out = os.path.join(tmp, "kc3_mux.ts")
    run_script("""
a = once(single("%s"))
v = once(single("%s"))
let {audio} = source.tracks(a)
let {video} = source.tracks(v)
s = source({audio=audio, video=video})
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %%ffmpeg(format="mpegts", %%audio.copy, %%video.copy), "%s", s)
o.on_stop(synchronous=true, shutdown)
""" % (fixture("gop.mp4"), fixture("midgop.mp4"), out))
    pkts = packets(out)
    c.check("kc3: different sources, both streams present",
            len(pkts["audio"]) > 0 and len(pkts["video"]) > 0)
    c.check("kc3: different sources, video starts on a keyframe", pkts["video"][0]["key"])
    c.check("kc3: different sources, audio not delayed by the video's wait",
            pkts["audio"][0]["dts"] < pkts["video"][0]["dts"] - Fraction(3, 10))

    # A source whose video ends before its first keyframe, then a normal one.
    out = os.path.join(tmp, "kc3_nokey.ts")
    run_script(copy_script(["nokey.mp4", "gop.mp4"], "%audio.copy, %video.copy", out, "mpegts"))
    pkts = packets(out)
    c.check("kc3: the output resumes on the next source",
            len(pkts["video"]) == len(first["video"]) and pkts["video"][0]["key"])

    # A keyframe that comes after the wait bound: the ready members start at
    # the bound and keep what they held.
    late = packets(fixture("latekey.mp4"))
    out = os.path.join(tmp, "kc3_latekey.ts")
    run_script(copy_script(["latekey.mp4"], "%audio.copy, %video.copy", out, "mpegts"))
    pkts = packets(out)
    c.check("kc3: audio starts at the wait bound",
            len(pkts["audio"]) >= len(late["audio"]) * 9 // 10)
    c.check("kc3: video starts later on its own keyframe",
            len(pkts["video"]) > 0 and pkts["video"][0]["key"])

    # A switch to a continuing source waits for no keyframe.
    out = os.path.join(tmp, "kc3_continuing.ts")
    produce("continuing", "mpegts", out, ["gop.mp4"])
    pkts = packets(out)
    c.check("kc3: no packet discarded at a continuing switch",
            len(pkts["video"]) == len(first["video"])
            and len(pkts["audio"]) == len(first["audio"]))


# known-complexity.md §5: packets with no DTS or a non-increasing DTS.
def kc5(c, tmp):
    plays = 2
    src = packets(fixture("gop.mp4"))
    n_a, n_v = len(src["audio"]), len(src["video"])

    def run(scenario, fmt="mpegts"):
        out = os.path.join(tmp, "kc5_%s.%s" % (scenario, fmt))
        produce(scenario, fmt, out, ["gop.mp4"] * plays)
        pkts = packets(out)
        for kind in pkts:
            assert_monotonic(c, "kc5 %s %s" % (scenario, kind), pkts[kind])
        return pkts

    pkts = run("nodts_audio")
    c.check("kc5: audio packets without DTS are written",
            len(pkts["audio"]) == n_a * plays)
    # Each video packet is an item fault and each run of them restarts the
    # group: what is binding is that nothing goes out of decode order and
    # that the output keeps streaming to the end of its input.
    pkts = run("nodts_video")
    # Size differences, since the container may add a constant header
    # (ADTS in MPEG-TS) to each packet.
    def shape(pkts):
        return [b["size"] - a["size"] for a, b in zip(pkts, pkts[1:])]

    c.check("kc5: the output keeps running to the end without video DTS",
            shape(pkts["audio"][-20:]) == shape(src["audio"][-20:]))
    c.check("kc5: no DTS taken from PTS on a stream with reordering",
            not any(p["pts"] == p["dts"] for p in pkts["video"]))
    pkts = run("equal_dts")
    c.check("kc5: an equal DTS drops that packet only",
            len(pkts["video"]) == (n_v - 1) * plays)
    pkts = run("backward_dts")
    c.check("kc5: backward DTS drop those packets only",
            len(pkts["video"]) == (n_v - 3) * plays)
    pkts = run("restart_dts")
    c.check("kc5: a source whose DTS restarts is carried again",
            len(pkts["video"]) >= plays * (30 + 20))
    pkts = run("close_dts", "flv")
    c.check("kc5: DTS closer than one container unit are written",
            len(pkts["video"]) == n_v * plays)


CHECKS = {"kc1": kc1, "kc2": kc2, "kc3": kc3, "kc5": kc5}

ENCODER = "src/core/optionals/ffmpeg/lib/encoder/copy_encoder.ml"

# (check, mutation, file, original text, mutated text)
MUTATIONS = [
    ("kc1", "DTS synthesised from positions", ENCODER,
     "        | Some d, Some p -> (d, p)",
     "        | Some _, Some _ -> (h.position - t.skipped - offset, h.position - t.skipped - offset)"),
    ("kc1", "separate PTS and DTS offsets", ENCODER,
     "    let dts' = dts + offset and pts' = pts + offset in",
     "    let dts' = dts + offset and pts' = pts + offset + (offset mod 7919) in"),
    ("kc1", "a join end taken as the last DTS without its duration", ENCODER,
     "    let extent = Option.value ~default:h.extent h.duration in",
     "    let extent = 0 in"),
    ("kc2", "one offset per stream", ENCODER,
     "          (fun ((tenure : tenure), _) -> tenure.offset <- Some offset)",
     "          (fun ((tenure : tenure), c) -> ignore offset; tenure.offset <- Some c)"),
    ("kc2", "offset taken from the first stream to switch", ENCODER,
     "        let offset = List.fold_left (fun o (_, c) -> max o c) c l in",
     "        let offset = ignore l; c in"),
    ("kc5", "packets without DTS dropped", ENCODER,
     "        | None, Some p -> (p, p)",
     "        | None, Some _ -> raise (Dropped \"no DTS\")"),
    ("kc5", "a non-increasing DTS written to the muxer", ENCODER,
     '      | Some last when dts = last -> raise (Dropped "equal DTS dropped")',
     "      | Some last when dts = last -> ()"),
    ("kc5", "a missing DTS filled from PTS on a reordering stream", ENCODER,
     "        | None, Some _ when track.reordering ->",
     "        | None, Some _ when false ->"),
    ("kc3", "one readiness shared by tracks of different sources", ENCODER,
     "        | Some tenure when tenure.source_id = g.id -> Some (track, tenure, true)",
     "        | Some tenure -> Some (track, tenure, true)"),
    ("kc3", "a keyframe wait on a declared discontinuity whose bitstream continues",
     ENCODER,
     "    track.keyframe = `Wait_for_keyframe && (not track.intra) && not continues",
     "    track.keyframe = `Wait_for_keyframe && (not track.intra) && ignore continues = ()"),
    ("kc3", "a keyframe wait with no bound", ENCODER,
     "        else if members <> [] && waited then (",
     "        else if members <> [] && waited && false then ("),
    ("kc3", "no keyframe wait", ENCODER,
     "    track.keyframe = `Wait_for_keyframe && (not track.intra) && not continues",
     "    false"),
]


def run_check(name):
    c = Counter()
    with tempfile.TemporaryDirectory(dir=FIXTURES) as tmp:
        CHECKS[name](c, tmp)
    if c.assertions == 0:
        raise CheckFailed("no assertion ran")
    return c.assertions


def build():
    subprocess.run(["dune", "build", "src/bin/liquidsoap.exe",
                    "src/core/optionals/ffmpeg/test/copy_producer.exe"],
                   cwd=ROOT, check=True, capture_output=True)


def with_mutation(path, old, new, f):
    path = os.path.join(ROOT, path)
    original = open(path).read()
    if old not in original:
        raise RuntimeError("mutation anchor not found in %s: %r" % (path, old))
    try:
        open(path, "w").write(original.replace(old, new, 1))
        build()
        return f()
    finally:
        open(path, "w").write(original)


def main(argv):
    mutations = "--mutations" in argv
    names = [a for a in argv if not a.startswith("--")] or list(CHECKS)
    fixtures()
    build()
    failures = 0
    for name in names:
        try:
            n = run_check(name)
            print("pass %s (%d assertions)" % (name, n))
        except CheckFailed as e:
            failures += 1
            print("FAIL %s: %s" % (name, e))
    if mutations:
        for check, mutation, path, old, new in MUTATIONS:
            if check not in names:
                continue
            try:
                n = with_mutation(path, old, new, lambda: run_check(check))
                failures += 1
                print("FAIL negative control %s / %s: passed (%d assertions)"
                      % (check, mutation, n))
            except CheckFailed as e:
                print("pass negative control %s / %s: %s" % (check, mutation, e))
        build()
    print("checks: %d, failures: %d" % (len(names), failures))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
