#!/usr/bin/env node
// Concurrent ffprobe load test for output.harbor.
//
// Spawns CONCURRENCY ffprobe processes against a streaming URL, staggered by
// STAGGER_MS. Each probe connects as a listener, reads enough data to identify
// the codec, then exits. Validates that every listener receives a valid audio
// stream. Exits non-zero if any probe fails.
//
// Environment:
//   PORT        Port to probe (default: 9900)
//   CONCURRENCY Number of concurrent listeners (default: 30)
//   STAGGER_MS  Milliseconds between each connection start (default: 200)

"use strict";

const { spawn } = require("child_process");

const PORT = process.env.PORT || "9900";
const CONCURRENCY = parseInt(process.env.CONCURRENCY || "30", 10);
const STAGGER_MIN_MS = parseInt(process.env.STAGGER_MIN_MS || "200", 10);
const STAGGER_MAX_MS = parseInt(process.env.STAGGER_MAX_MS || "2000", 10);

function ts() {
  return new Date().toISOString().slice(11, 23);
}

function probe(url, index) {
  return new Promise((resolve) => {
    const start = Date.now();
    console.log(`[${ts()}] listener ${index}: starting`);
    const proc = spawn("ffprobe", [
      "-v",
      "quiet",
      "-probesize",
      "64k",
      "-analyzeduration",
      "2000000",
      "-print_format",
      "json",
      "-show_streams",
      url,
    ]);

    let stdout = "";
    proc.stdout.on("data", (chunk) => {
      stdout += chunk;
    });
    proc.on("close", (code) => {
      const elapsed = Date.now() - start;
      try {
        const info = JSON.parse(stdout);
        const audio = (info.streams || []).find(
          (s) => s.codec_type === "audio",
        );
        if (audio) {
          console.log(
            `[${ts()}] listener ${index}: ok (${audio.codec_name}, ${elapsed}ms)`,
          );
          resolve({ index, ok: true, codec: audio.codec_name });
        } else {
          console.error(
            `[${ts()}] listener ${index}: no audio stream (exit ${code}, ${elapsed}ms)`,
          );
          resolve({
            index,
            ok: false,
            error: `no audio stream (exit ${code})`,
          });
        }
      } catch (_) {
        console.error(
          `[${ts()}] listener ${index}: could not parse ffprobe output (exit ${code}, ${elapsed}ms)`,
        );
        resolve({
          index,
          ok: false,
          error: `could not parse ffprobe output (exit ${code})`,
        });
      }
    });
    proc.on("error", (err) => {
      const elapsed = Date.now() - start;
      console.error(
        `[${ts()}] listener ${index}: spawn error: ${err.message} (${elapsed}ms)`,
      );
      resolve({ index, ok: false, error: err.message });
    });
  });
}

async function main() {
  const url = `http://localhost:${PORT}/test`;
  console.log(
    `Probing ${url} with ${CONCURRENCY} concurrent listeners (${STAGGER_MIN_MS}-${STAGGER_MAX_MS}ms random stagger)...`,
  );

  const promises = [];
  for (let i = 0; i < CONCURRENCY; i++) {
    if (i > 0) {
      const delay =
        STAGGER_MIN_MS + Math.random() * (STAGGER_MAX_MS - STAGGER_MIN_MS);
      await new Promise((r) => setTimeout(r, delay));
    }
    promises.push(probe(url, i + 1));
  }

  const results = await Promise.all(promises);
  const passed = results.filter((r) => r.ok);
  const failed = results.filter((r) => !r.ok);

  console.log(
    `\n${passed.length}/${CONCURRENCY} listeners received valid audio`,
  );
  if (passed.length > 0) {
    const codec = passed[0].codec;
    console.log(`  codec: ${codec}`);
  }
  if (failed.length > 0) {
    console.error(`${failed.length} listeners failed:`);
    failed.forEach((r) => console.error(`  listener ${r.index}: ${r.error}`));
    process.exit(1);
  }
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
