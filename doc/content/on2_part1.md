This part starts from scratch. All you need is a working install
of liquidsoap.
If you don't have one already, we can help you;
but if you haven't, check the [corresponding page](download.html).

# Run liquidsoap on one-liners

A good way to test your install and get started
is to execute very small liquidsoap programs.
Such ``one-liners'' are also often useful to accomplish simple tasks.

## Play a synthesized sound

Simply execute the following command,
and you should hear a 440Hz sound on your soundcard:

```
liquidsoap 'output(sine())'
```

Did it work? If so, try to modify it:

- Change the pitch. Hint: get the doc of `sine` using `liquidsoap -h sine` or [online](reference.html).
- Use a different wave shape, or perhaps some white noise. Hint: look-up the [API](reference.html) in the `Source / Input` section.

## Play a remote stream, discover fallibility

Try to execute the liquidsoap expression:

```{.liquidsoap include="on2-remote.liq"}

```

You should now be listening to [Pi-Radio](http://piradio.de),
unless you have no network connection, in which case the
`input.http(...)` source _fails_.

The output operator `output` is okay with failure: it simply plays silence
when the source fails, waiting for it to be ready again.
But some operators are stricter.
If you try

```{.liquidsoap include="on2-fallible.liq"}

```

the pulseaudio output (which uses libao to access your soundcard)
warns you that there might be a failure.
To ignore this problem, pass `fallible=true` to the output.

This might seem annoying
but it can make sense if you have listeners and you want to make
sure that your stream is always up and running for them.

## Play a list of files

The `playlist` operator can be used to build a source that plays
a list of files. The resulting source will be fallible.
You can either pass a directory name, or a text file containing the
list of files.
There are lots of possibilities here, but for now just look
at the `mode` parameter; we'll learn more later.

## An interactive example

Suppose we want to be able to request a particular file for playout
instead of the automatically chosen files of the playlist.
This is achieved by wrapping the playlist in a fallback choice
with a request queue:

```{.liquidsoap include="on2-interactive.liq"}

```

If you run this example, you'll hear your playlist, because the queue
is empty. The queue can be fed through _server commands_. Enable the
telnet server interface by passing `-t` on liquidsoap's command line,
and connect to it using `telnet localhost 1234`.

- Type `help`, find the command for pushing a request (by its file name) in the queue, and try it.
- Find the command for skipping the current track. If you skip a playlist track after having pushed a request in the queue, you should hear that request -- unless the request failed to be prepared.
- Find the command for listing the next requests in queue. They are given by their request id (RID); the commands `request.metadata` and `request.trace` give you info from the RID.
- Also notice commands for listing the next files to be played by the playlist (depends on the playlist more), and reloading the playlist.
- Try setting `track_sensitive=false` for the fallback, see what it does (better, guess what it does from the doc).

## Encode a file

You have learned how to build a few sources, synthesizing sound from scratch,
from a remote stream or from a list of files. Now, instead of playing the
stream directly to your soundcard, we'll encode it and save it to a local
file:

```{.liquidsoap include="on2-file-output.liq" from="BEGIN"}

```

Here `source` is whatever you want, for example `sine()`.

You can tweak the options of the encoding format, or change the encoding
format; the available options are listed [here](encoding_formats.html).
You can change the number of channels (for example, using `%vorbis(mono)`)
but this may create problems with a playlist or remote stream,
because conversions are not implicit in liquidsoap;
we'll see later how to deal with them.

## Icecast output

You can now easily change the file example to send you stream to an
icecast server: simply use `output.icecast` instead of `output.file`,
passing a `mount` parameter instead of a file name,
and perhaps overriding the defaults for `host` (`"127.0.0.1"`)
and `password` (`"hackme"`).
You just created your first Internet radio using liquidsoap!

If you have control over the icecast server (or over the network link to that
server) you can simulate a loss of connection. Notice that liquidsoap only
attempts once to reconnect, then fails and shuts down. For another behavior
that tolerates more persistent failures, set `restart=true`.

# Using liquidsoap in production

One-liners are good for one-shot uses, but not the most convenient
for more complex liquidsoap programs, and for saving/editing the program.

## Running a script file

Write the interactive example expression in a file, say `test.liq`.

- You can run it using `liquidsoap -t - < test.liq`, you get the same behavior as before.
- If you run it using `liquidsoap -t test.liq` the logs will be written in `test.log` in the default logging directory. This can fail as you may not have access to that directory. Change the directory using the setting (see [how to get help](help.html) about that) `log.file.path`. Then use the settings `log.stdout` and `log.file` for logging to the terminal and not to a file.
- Finally, find the setting for getting rid of the `-t` option on the command line.
- You can also use `#!/usr/bin/liquidsoap` (adapt the path) as the first line of your script to directly run `./test.liq` instead of `liquidsoap test.liq` (you need to `chmod +x test.liq`).

You can also load several scripts and expression on the command line. The last
script or expression is taken as the main one, and determines the logging behavior of liquidsoap.

## Daemon mode

Finally, the `-d` command-line option (or `init.daemon` setting) triggers
the _daemon mode_ where liquidsoap detaches from the terminal to run in the
background.

## Checking a media file

To check how liquidsoap sees a file, you can run `liquidsoap -r <FILE>`.
Liquidsoap will attempt to decode the file and its metadata,
and compute its duration.
This is (almost) the same process as used during streaming,
so it can be used for checking how something works (or doesn't work).

# Get comfortable with the language

Although it's easy to forget it when using simple liquidsoap expressions,
liquidsoap is a rich programming language.
Below is a list of simple exercises to get more comfortable with it.
Those exercises do not deal with sources and even less with radio,
and might seem pretty dull. But it's useful to go through them and
try to understand what's going on: it will allow you to avoid the
most common mistakes later,
when trying to write more complex liquidsoap scripts.

You can write the following examples in a script and execute it, or
type them directly in your terminal, followed by ctrl-D, on the
standard input of `liquidsoap -`.
You can also pass `-c` so that liquidsoap does not warn you that
it has no source to stream.

## Using variables

Run the following script:

```{.liquidsoap include="on2-print.liq"}

```

As follows:

```
% liquidsoap --no-stdlib -i /path/to/script.liq
x     : int
42
No output defined, nothing to do.
```

Try to obtain the following types (some help can be found [there](language.html)):

- `float`
- `bool`
- `string`
- `[string]` (list of strings)
- `(bool*string)` (a pair made of a boolean and a string)

You can redefine a variable:

```{.liquidsoap include="on2-redefine.liq"}

```

## Defining a function

Try this:

```{.liquidsoap include="on2-function.liq"}

```

Change `^` for `+`. Liquidsoap will complain that it cannot add strings;
additions are only for numbers (integers and floats). Adapt the last line
to fix that problem.

## Conditionals

A simple example:

```{.liquidsoap include="on2-if.liq"}

```

It can also be written as follows:

```{.liquidsoap include="on2-if2.liq"}

```

Now, define the variable `message` to be the correct message depending
on the test, and finish by `print(message)`.
To do this, keep in mind the following:
A variable definition is local to the current scope.
Redefining a new variable does not erase or override previous definitions
but only masks them in the current scope.
In other words, _definitions_ should not be confused with
_assignments_ (which are performed by `x=...` in non-functional languages).
You'll learn later how to use assignments when you really need them.

## Sequencing, returning

Here is a function that prints the date and returns 42:

```{.liquidsoap include="on2-seq.liq"}

```

Note that there is no return statement in liquidsoap (in fact there is
no "statement" at all). Every expression evaluates to a value. A sequence
evaluates to the value of its last expression (`42` in the body of the function
`f`, and `print(1+f())` in the full program).
The value "returned" by a function is simply the result of evaluating its
body.

## Labels and optional parameters

In liquidsoap, functions arguments can be labeled or not.
For example, in `f(x,y,foo=z)` we pass `x` and `y` as the first two
unlabeled arguments, and `z` for the argument labeled `foo`.
Moreover, labeled arguments can be optional:
you saw this with most examples in this page, where each operator
had lots of parameters that you didn't set (for example,
`track_sensitive` in `fallback`).

When reading the doc of a function, you see the type of the function,
followed by a description of its arguments. Often you can ignore the
type, but when you write an incorrect script, you might need to read
types to understand error messages. A function type is written
`(A1,..,AN)->T` where `T` is the type of values returned by the function
and each `Ai` specifies one parameters:

- unlabeled parameters are simply given by their type (for example, string concatenation has type `(string,string)->string`);
- mandatory labeled parameters are written `label:T` where `T` is the type of the parameter;
- optional labeled parameters are written `?label:T`.

You will rarely have to define a function with labeled parameters,
but if you're curious you can learn it [there](language.html).

## What you cannot do

Liquidsoap does not have `while` and `for` loops, nor recursion.
This is mostly because they are not really needed (yet...), notably
since functions like `list.map` and `list.iter` are often a good replacement.
