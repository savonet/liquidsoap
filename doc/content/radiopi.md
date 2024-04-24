# RadioPi

[RadioPi](http://www.radiopi.org) is the web radio of the ECP (Ecole Centrale de Paris). RadioPi runs many channels.
There are topical channels (Reggae, Hip-Hop, Jazz, ...). On top of that, they periodically broadcast live shows,
which are relayed on all channels.

We met a RadioPi manager right after having released Liquidsoap 0.2.0, and he was seduced by the system. They needed
quite complex features, which they were at that time fulfilling using dirty tricks, loads of obfuscated scripts.
Using Liquidsoap now allow them to do all they want in an integrated way, but also provided new features.

### The migration process

Quite easy actually. They used to have many instances Ices2, each of these calling a Perl script to get the next song.
Other scripts were used for switching channels to live shows.

Now they have this single Liquidsoap script, no more. It calls external scripts to interact with their web-based song
scheduling system. And they won new features: blank detection and distributed encoding.

The first machine gets its files from a ftp server opened on the second machine.
Liquidsoap handles download automatically.

Each file is given by an external script, `radiopilote-getnext`,
whose answer looks as follows (except that it's on a single line):

```
annotate:file_id="3541",length="400.613877551",\
  type="chansons",title="John Holt - Holigan",\
  artist="RadioPi - Canal reggae",\
  album="Studio One SeleKta! - Album Studio 1 12",\
  canal="reggae":ftp://***:***@host/files/3541.mp3
```

Note that we use annotate to pass some variables to liquidsoap...

```{.liquidsoap include="radiopi.liq"}

```

The other machine has a similar configuration except that files are local, but this is exactly the same for liquidsoap !

Using harbor, the live connects directly to liquidsoap, using port `8000` (icecast runs on port `8080`).
Then, liquidsoap starts a relay to the other encoder, and both switch their channels to the new live.

Additionally, a file output is started upon live connection, in order to backup the stream. You could also add a relay to
icecast in order to manually check what's received by the harbor.
