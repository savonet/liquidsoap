# Liquidsoap 1.5.0

## Forewords

Installing liquidsoap can be a difficult task. The software relies on a up-to date
`OCaml` compiler, as well as a bunch of `OCaml` modules and, for most of them, corresponding
C library dependencies.

Our recommended way of installing liquidsoap is via [opam](http://opam.ocaml.org/). `opam` can take
care of install the correct `OCaml` compiler, optional and required dependencies as well as system-specific
package dependencies.

The `opam` method is described in details in the [documentation](doc/content/install.md).
We recommend that any interested user head over to this link to install the software via `opam`.

The following of this document describes how to install the software via its `configure` script and is
intended either for system administrators or package maintainers.

## Dependencies

Below is a list of dependencies, mostly OCaml libraries. Optional libraries
provide extra features. They need to be detected by the `configure` script.

Most of the libraries are developed by the Savonet project and, in addition to
being available through traditional distribution channels, are bundled in the
[liquidsoap-&lt;version&gt;-full.tar.bz2](https://github.com/savonet/liquidsoap/releases)
tarballs for easier builds.

Libraries not developed by Savonet are:

- camlimages
- camomile
- gd4o
- ocaml-pcre
- ocaml-magic
- tsdl / tsdl-ttf / tsdl-image
- yojson

### Mandatory dependencies:

| Dependency     | Version                   |
| -------------- | ------------------------- |
| OCaml compiler | >= 4.08.0     |
| ocaml-dtools   |  >= 0.4.2    |
| ocaml-duppy    |  >= 0.8.1     |
| ocaml-mm       |  >= 0.6.0        |
| ocaml-pcre     |       |
| menhir         |  |
| sedlex         |  >= 2.0    |

### Recommended dependencies:

| Dependency       | Version                    | Functionality                  |
| ---------------- | -------------------------- | ------------------------------ |
| camomile         |  >= 1.0.0   | Charset recoding in metadata   |
| ocaml-samplerate |  >= 0.1.5 | Libsamplerate audio conversion |

### Optional dependencies:

| Dependency          | Version                              | Functionality                                 |
| ------------------- | ------------------------------------ | --------------------------------------------- |
| camlimages          |            | Image decoding                                |
| gd4o                |                    | Video.add_text() on servers without X         |
| ocaml-alsa          |  >= 0.3.0                 | ALSA I/O                                      |
| ocaml-ao            |  >= 0.2.0                   | Output via libao                              |
| ocaml-bjack         |  >= 0.1.3                | Jack support                                  |
| ocaml-cry           |  >= 0.6.5                  | Sending to Shoutcast & Icecast                |
| ocaml-dssi          |  >= 0.1.3                 | DSSI sound synthesis                          |
| ocaml-faad          |  >= 0.5.0                 | AAC stream decoding                           |
| ocaml-fdkaac        |  >= 0.3.1               | AAC(+) encoding                               |
| ocaml-ffmpeg        |  >= 0.5.0        | Audio and video tools from the ffmpeg library | 
| ocaml-flac          |  >= 0.2.0                 | Flac and Ogg/Flac codec                       |
| ocaml-frei0r        |  >= 0.1.0               | Frei0r plugins                                |
| ocaml-gavl          |  >= 0.1.4                 | Video conversion using the gavl library       |
| ocaml-gstreamer     |  >= 0.3.0            | GStreamer input, output and encoding/decoding |
| ocaml-inotify       |  >= 1.0              | Reloading playlists when changed              |
| ocaml-ladspa        |  >= 0.2.0               | LADSPA plugins                                |
| ocaml-lame          |  >= 0.3.4                 | MP3 encoding                                  |
| ocaml-lastfm        |  >= 0.3.0               | Lastfm scrobbling                             |
| ocaml-lo            |  >= 0.1.2                   | OSC (Open Sound Control) support              |
| ocaml-mad           |  >= 0.5.0                  | MP3 decoding                                  |
| ocaml-magic         |  >= 0.6                | File type detection                           |
| ocaml-ogg           |  >= 0.6.0                  | Ogg codecs                                    |
| ocaml-opus          |  >= 0.1.3                 | Ogg/Opus codec                                |
| ocaml-portaudio     |  >= 0.2.0            | Portaudio I/O                                 |
| ocaml-pulseaudio    |  >= 0.1.4           | PulseAudio I/O                                |
| ocaml-shine         |  >= 0.2.0                | Fixed-point MP3 encoding                      |
| ocaml-soundtouch    |  >= 0.1.9           | Libsoundtouch's audio effects                 |
| ocaml-speex         |  >= 0.3.0                | Ogg/Speex codec                               |
| ocaml-srt           |  >= 0.1.1                  | SRT I/O                                       |
| ocaml-ssl           |  >= 0.5.2                  | SSL/https support                             |
| ocaml-taglib        |  >= 0.3.0               | MP3ID3 metadata access                        |
| ocaml-theora        |  >= 0.3.1               | Ogg/Theora codec                              |
| ocaml-vorbis        |  >= 0.7.0               | Ogg/Vorbis codec                              |
| ocaml-xmlplaylist   |  >= 0.1.3          | XML-based playlist formats                    |
| osx-secure-transport|  | SSL/https support via OSX's SecureTransport   |
| posix-time2         |           | High-resolution time/latency                  |
| tsdl                |                  | Display videos                                |
| tsdl-image          |            | Load images                                   |
| tsdl-tff            |              | Render fonts                                  |
| yojson              |                | Parsing JSON data (of_json function)          |

### Runtime optional dependencies:

| Dependency          | Functionality                                     |
| ------------------- | ------------------------------------------------- |
| awscli              | `s3://` and `polly://` protocol support           |
| curl                | `http`/`https`/`ftp` protocol support             |
| ffmpeg              | external I/O, `replay_gain` level computation, .. |
| youtube-dl          | youtube video and playlist support                |

    
## Installing via configure

The build processus starts with by invoking the `configure` script:

```
% ./configure
```

If you want a complete installation of liquidsoap, enabling a production use of
liquidsoap as a daemon, you should pass `--with-user=<login>` and
`--with-group=<group>` options to indicate which user/group you have created for
liquidsoap.

Then, build the software:

```
% make
```

You can also generate the documentation for liquidsoap:

```
% make doc
```

It will generate the HTML documentation, including a version of the scripting
API reference corresponding to your configuration.

Then, you may proceed to the installation. You may need to be root for that.

```
% make install
```

This will not install files such as `/var/log/liquidsoap` unless you have provided
a user/group under which liquidsoap should be ran. This behavior can be
overridden by passing `INSTALL_DAEMON="yes"` (useful for preparing binary
packages).


If you need to run liquidsoap as a daemon, you can then have a look at
[liquidsoap-daemon](https://github.com/savonet/liquidsoap-daemon).
