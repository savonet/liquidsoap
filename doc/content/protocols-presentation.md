# Protocols in Liquidsoap

When Liquidsoap plays a track, it doesn’t just _magically_ appear — it has to be **resolved** from somewhere. That “somewhere” could be a local file, a remote URL, a database entry, or even something generated on the fly.

That’s where **protocols** come in.
Protocols are little rules that tell Liquidsoap:

> “If you see a request in the form `protocol:arguments`, here’s how to turn it into a real file or stream you can play.” 🎯

For example:

```text
http://www.example.com/song.mp3
say:Hello world!
s3://my-bucket/path/to/file.mp3
```

In each case, the prefix before the `:` is the protocol, and the part after it is the argument passed to your resolver code.

## Built-in and custom protocols

Liquidsoap already ships with many ready-made protocols, written in the Liquidsoap scripting language. You can explore them in the [protocol reference](protocols.html). But the real power comes when you define your own.

## The anatomy of a protocol

Every protocol is defined by a **handler function**. The handler:

1. Accepts the protocol arguments and some extra helper parameters.
2. Returns a single resolved URI
3. Can call other protocols in sequence, building a chain of transformations.

The function always gets two special variables:

- **`~rlog`** → A logging function. Use it to write debug or info messages that stay attached to the request.
- **`~maxtime`** → A UNIX timestamp after which your resolver should give up.

## The `process.uri` helper 🛠️

Before diving into the examples, it’s important to understand **`process.uri`**, a convenient helper for creating URIs of the form:

```text
process:<binary> <arguments>
```

When Liquidsoap encounters such a URI, it will automatically execute the given command and cancel it if it exceeds `~maxtime`.

If you provide a `uri` argument, Liquidsoap will first resolve that URI to a local file. Your command can then use two placeholders:

- **`$(input)`** → replaced with the local file resolved from the `uri` argument (only if `uri` is provided).
- **`$(output)`** → replaced with the path to a temporary file whose extension is taken from the `extname` argument.

⚠️ The output file is created **empty** before the command runs, to prevent race conditions on file ownership. This means your process must be able to overwrite it.

By using `process.uri`, you can safely wrap external commands in a way that’s time-bound, predictable, and integrates smoothly into Liquidsoap’s request resolution chain.

## Example 1 — Fetching from S3 ☁️

Let’s say your files live on Amazon S3, and you want Liquidsoap to fetch them on demand:

```liquidsoap
def s3_protocol(~rlog, ~maxtime, arg) =
  extname = file.extension(leading_dot=false, dir_sep="/", arg)
  process.uri(extname=extname,
              "aws s3 cp s3:#{arg} $(output)")
end

protocol.add("s3", s3_protocol,
             doc="Fetch files from S3 using the AWS CLI",
             syntax="s3://bucket/path/to/file")
```

Now a request like:

```text
s3://my-bucket/song.mp3
```

will be downloaded locally and returned as the playable URI.

## Example 2 — Database lookup 📀

Protocols can also be dynamic. For instance, you might store file paths in a database keyed by track IDs:

```liquidsoap
def db_lookup_protocol(~rlog, ~maxtime, arg) =
  string.trim(process.read("psql -t -c 'SELECT path FROM tracks WHERE id=#{int_of_string(arg)};'"))
end

protocol.add("db_lookup", db_lookup_protocol,
             doc="Fetch file path from database by track ID")
```

Now you can request:

```text
db_lookup:42
```

and Liquidsoap will resolve it via your database.

## Example 3 — Adding preprocessing

Want to normalize audio before playing? Or apply a voice-over?

```liquidsoap
def normalize_protocol(~rlog, ~maxtime, arg) =
  process.uri(extname="wav",
              uri=arg,
              "normalize-audio $(input) $(output)")
end

protocol.add("normalize", normalize_protocol,
             doc="Normalize audio levels before playback")
```

You can chain protocols too:

```text
normalize:cue_cut:s3://my-bucket/file.mp3
```

Liquidsoap will fetch from S3 → cut the segment → normalize it → play.

## Chaining magic

The real beauty of protocols is chaining. Each protocol resolves to a single URI, which can then be handed off to the next one in the chain. This means you can build complex request pipelines:

```text
voiceover:normalize:db_lookup:1234
```

💡 Here, `db_lookup` fetches a path, `normalize` evens out the audio, and `voiceover` mixes in an announcement.

## Tips for writing robust protocols

- Always **respect `~maxtime`** to avoid long-hanging processes.
- Use `~rlog` generously for debugging:

  ```liquidsoap
  rlog("Downloading from S3: #{arg}")
  ```

- Keep your commands secure — if you interpolate `arg` into a shell command, validate or escape it.
- Test each piece of the chain independently before combining them.

By mastering protocols, you’re not just telling Liquidsoap where to find your content — you’re **designing the path it takes to get there**. That’s a superpower in streaming workflows, letting you pull from anywhere, process in any way, and still keep things flowing smoothly. 🚀
