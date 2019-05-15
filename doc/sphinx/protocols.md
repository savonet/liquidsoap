Liquidsoap protocol reference
=============================
#### Annotate
* Syntax: `annotate:key="val",key2="val2",...:uri`
* Static: `false`

Add metadata to a request

#### Ffmpeg2wav
* Syntax: `ffmpeg2wav:uri`
* Static: `false`

Decode any file to wave using ffmpeg

#### Ftp
* Syntax: `ftp://...`
* Static: `false`

Download files using curl

#### Http
* Syntax: `http://...`
* Static: `false`

Download files using curl

#### Https
* Syntax: `https://...`
* Static: `false`

Download files using curl

#### Mpd
* Syntax: `mpd:tag=value`
* Static: `false`

Finds all files with a tag equal to a given value using mpd.

#### Polly
* Syntax: `polly:Text to read`
* Static: `true`

Generate speech synthesis using AWS polly service. Result might be mono, needs aws binary in the path.

#### Process
* Syntax: `process:<extname>,<cmd>[:uri]`
* Static: `false`

Resolve a request using an arbitrary process. `<cmd>` is interpolated with: ```
[("input",<input>),("output",<output>),("colon",":")]```
. `uri` is an optional child request, `<output>` is the name of a fresh temporary file and has extension `.<extname>`. `<input>` is an optional input file name as returned while resolving `uri`.

#### Replay_gain
* Syntax: `replay_gain:uri`
* Static: `false`

Compute replaygain value using the extract-replaygain script. Adds returned value as `"replay_gain"` metadata

#### S3
* Syntax: `s3://uri`
* Static: `false`

Fetch files from s3 using the AWS CLI

#### Say
* Syntax: `say:Text to read`
* Static: `true`

Generate speech synthesis using text2wave and sox. Result is always stereo.

#### Text2wave
* Syntax: `text2wav2:Text to read`
* Static: `true`

Generate speech synthesis using text2wave. Result may be mono.

#### Tmp
* Syntax: `tmp:uri`
* Static: `false`

Mark the given uri as temporary. Useful when chaining protocols

#### Youtube-dl
* Syntax: `youtube-dl:uri`
* Static: `false`

Resolve a request using youtube-dl.

#### Youtube-pl
* Syntax: `youtube-pl:uri`
* Static: `false`

Resolve a request as a youtube playlist using youtube-dl.


