Basics
======
Starting with version `1.0.1`, it is possible to build a liquidsoap binary that can load
all its dependencies from any arbitrary path. This is very useful to distribute a liquidsoap
bundled binary, independent of the distribution used.

You can enable custom path at configure time, by passing the `--enable-custom-path` configuration option.
A custom loading path is a directory that contains the following file/directories:

* `./camomile`: Camomile shared data. They are usually located in `/usr/(local/)share/camomile`
* `./libs`: pervasive scripts. Their are located in `liquidsoap/scripts` in liquidsoap's sources
* `./log`: default log directories
* `./magic`: directory for magic files. See below for more details.
* `./plugins`: default plugins directory (most likely empty)
* `./run`: default runtime files directory

Adding liquidsoap binary
========================
In order to ship a liquidsoap binary which is independent of the distribution it will
be run on, one need to also include its dynamic libraries, except for the most common.
The following command may be used to list them:

```
ldd ./liquidsoap | grep usr | cut -d' ' -f 3
```

Those libraries are usually copied into a `./ld` directory. Then, the `LD_LIBRARY_PATH`
is used to point the dynamic loader to this directory.

Finally, the `liquidsoap` library is usually added in `./bin/liquidsoap`

Configuration variables
=======================
In the following, configuration variables may refer to either absolute or relative paths. If referring to
a relative path, the path is resolved relatively to the directory where the `liquidsoap` binary
is located at.

In order to tell liquidsoap where its custom path is located, you need to set the
`LIQUIDSOAP_BASE_DIR`.

Another important variable is `MAGIC`. It tells liquidsoap where to load the libmagic's
definitions and defaults to `../magic/magic.mgc`. Older versions of libmagic may
require to use `magic/magic.mime` instead.

Full example
============
For a fully-functional example, you can check our [heroku buildpack](https://github.com/savonet/heroku-buildpack-liquidsoap).
Its layout is:

```
./bin
./bin/liquidsoap
./camomile
./camomile/charmaps
(...)
./ld
./ld/libao.so.2
(...)
./libs
./libs/externals.liq
(...)
./log
./magic
./magic/magic.mime
./plugins
./run
```

Its configuration variables are set to:

```
LD_LIBRARY_PATH=/path/to/ld
LIQUIDSOAP_BASE_DIR=..
MAGIC=../magic/magic.mime
```

As you can see, we use an old version of `libmagic` so we need to load `magic.mime` instead of `magic.mgc`.
