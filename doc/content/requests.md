An abstract notion of files: requests
=====================================
The request is an abstract notion of file which can be conveniently used for defining powerful sources. A request can denote a local file, a remote file, or even a dynamically generated file. They are resolved to a local file thanks to a set of *protocols*. Then, audio requests are transparently decoded thanks to a set of audio and metadata *formats*.

The systematic use of requests to access files allows you to use remote URIs instead of local paths everywhere. It is perfectly OK to create a playlist for a remote list containing remote URIs: ```
playlist("http://my/friends/playlist.pls")```
.

The resolution process
----------------------
The nice thing about resolution is that it is recursive and supports backtracking. An URI can be changed into a list of new ones, which are in turn resolved. The process succeeds if some valid local file appears at some point. If it doesn't succeed on one branch then it goes back to another branch. A typical complex resolution would be:

* `bubble:artist="bodom"` * `ftp://no/where`  * `Error`


 * `ftp://some/valid.ogg`  * `/tmp/success.ogg`





On top of that, metadata is extracted at every step in the branch. Usually, only the final local file yields interesting metadata (artist,album,...). But metadata can also be the nickname of the user who requested the song, set using the `annotate` protocol.

At the end of the resolution process, in case of a media request,
liquidsoap checks that the file is decodable,
*i.e.*, there should be a valid decoder for it.

Each request gets assigned a request identifier (RID) which is used by
various sources to identify which request(s) they are using. Knowing
this number, you can monitor a request, even after it's been destroyed
(see setting `request.grace_time`). Two [server](server.html)
commands are available: `request.trace` shows a log of
the resolution process and `request.metadata` shows the
current request metadata. In addition, server commands are available
to obtain the list of all requests, alive requests, currently resolving
requests and currently playing requests (respectively
`request.all`,
`request.alive`,
`request.resolving`,
`request.on_air`).

Currently supported protocols
-----------------------------
* HTTP, HTTPS, FTP thanks to curl
* SAY for speech synthesis (requires festival): `say:I am a robot` resolves to the WAV file resulting from the synthesis.
* TIME for speech synthesis of the current time: ```
time: It is exactly $(time), and you're still listening.```

* ANNOTATE for manually setting metadata, typically used in ```
annotate:nick="alice",message="for bob":/some/track/uri```


The extra metadata can then be synthesized in the audio stream, or merged into the standard metadata fields, or used on a rich web interface...
It is also possible to add a new protocol from the script, as it is done with [Beets](beets.html) for getting songs from a database query.

Currently supported formats
---------------------------
* MPEG-1 Layer II (MP2) and Layer III (MP3) through libmad and `ocaml-mad`
* Ogg Vorbis through libvorbis and `ocaml-vorbis`
* WAV
* AAC
* and much more through external decoders!
