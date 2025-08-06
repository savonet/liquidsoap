# Understanding Latency in Liquidsoap

**What it means when you see: `We must catchup xxx sec`**

### What Is Latency?

To generate a media stream, Liquidsoap runs a **media streaming loop**. This loop creates small chunks of audio or video data (e.g. 0.04 seconds long) and sends them to your outputs.

If it takes **less than 0.04 seconds** to produce a 0.04s chunk, Liquidsoap is on time—or even a bit ahead—and it can rest briefly before producing the next chunk.

But if it takes **more than 0.04 seconds**, Liquidsoap starts to fall behind. When the delay grows too large, you’ll see a message like:

> **We must catchup 1.42 sec**

⏱️ This means the system is **too slow** and needs to produce data faster to get back on track.

### Who Controls Time?

Not all media setups behave the same. Some components control their own timing, while others rely on Liquidsoap to do it.

#### 1. `self-sync` sources (external time control)

Some sources or outputs are **self-synchronized**: they _block_ until it’s time to produce or consume data. These components manage their own timing, and Liquidsoap just follows along.

Examples of self-sync components:

- 🎧 Sound cards (e.g. `output.ao`, `input.alsa`, etc..)
- 🌐 Streaming inputs (e.g. `input.srt`, or `input.ffmpeg` (depending on its source))

In these cases, Liquidsoap doesn't need to check the clock—it just lets the source or output drive the latency.

#### 2. CPU-Controlled Latency (internal time control)

Other setups—like playing audio files or streaming to icecast—don’t manage time themselves. Liquidsoap must rely on the **CPU clock** to pace the streaming loop.

Examples:

- Audio files (`single`, `playlist`, etc.)
- Icecast or shoutcast streaming (`output.icecast`, `output.shoutcast`)

### Switching Between Time Sources 🔀

Your stream may involve switching between multiple types of sources. For example:

```liquidsoap
fallback([input.srt(...), single("music.mp3")])
```

In this case:

- `single` is CPU-controlled.
- `input.srt` is self-sync.

Liquidsoap has to decide **who’s in charge of the clock** at any given moment. It does this by looking at the sources
that will be used to produce data in the next round of the streaming loop.

For instance, in the above, only one of the two sources will ever be used to produce data. If it is `single`, the source
is CPU-controled, otherwise it is `self-sync`.

### Synchronization conflicts ⚠️

In some cases, you may have to fix synchronization conflicts. For instance:

```liquidsoap
output.ao(fallible=true, input.srt("..."))
```

can result in this error:

```
Error 17: clock input.srt has multiple synchronization sources. Do you need to set self_sync=false?

Sync sources:
 srt from source input.srt
 ao from source output.ao
```

In this case, you should let one of the source or output drive the other one:

```liquidsoap
output.ao(fallible=true, input.srt("...", self_sync=false))
```

This tells Liquidsoap to follow the timing of the AO output.

### Diagnosing Latency Issues 🧪

When you see a `"We must catchup"` message, here’s how to go about diagnosing the issue:

#### ✅ 1. Is latency CPU-controlled?

If no self-synchronized component is active, Liquidsoap must use the CPU clock to manage time. That means _any delay in computation_ causes the system to fall behind.

#### ✅ 2. Should a source be self-synchronized?

In some cases such as when using `input.ffmpeg`, you may need to manually set it as `self_sync`. Typically, `input.ffmpeg` should be self-sync
when decoding a `libsrt` or `rtmp` input.

#### ✅ 3. Can the system keep up?

If timing is CPU-controlled, then Liquidsoap needs to generate chunks fast enough to stay on schedule. If it can’t, you’ll see the catchup warning.

**Common culprits:**

- 🧮 CPU isn’t fast enough to decode/encode in real-time.
- 💽 Disk access is slow—especially with network-based filesystems like NFS.
- 🔄 Blocking code inside the streaming loop.

💡 **Pro Tip:** Before version 2.4.0, all callbacks in Liquidsoap were synchronous (blocking). Since 2.4.0, most callbacks are **asynchronous** by default.

#### ✅ 4. Are other processes slowing things down?

Sometimes it's not Liquidsoap's fault. Other system tasks—like `cron` jobs or background processes—can momentarily hog CPU or disk resources and cause temporary latency.

### Final Notes

Latency can be confusing at first, but it's usually about one of two things:

1. Liquidsoap is trying to manage time itself and falls behind. Your system might be too slow for the task at hand.
2. You forgot to let a source or output manage time (`self_sync`).

If you're still stuck:

- Enable detailed logs with `log.level = 4`
- Try simplifying your setup and adding pieces back one by one
