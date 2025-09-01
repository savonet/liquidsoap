---
title: LIQUIDSOAP
section: 1
date: Jul 24, 2019
header: Liquidsoap @version@
footer: Liquidsoap @version@
...

<!-- .TH LIQUIDSOAP 1 "Jul 1, 2016" "Liquidsoap @version@" -->

# NAME

liquidsoap - a multimedia streaming language

# SYNOPSIS

liquidsoap [ _options_ ] [ _script_ | _expression_ ]

# DESCRIPTION

Liquidsoap is a programming language for describing multimedia streaming systems.
It is very flexible, making simple things simple but giving a lot
of control for advanced uses. Liquidsoap
supports audio, video and MIDI streams,
and a wide range of input/output operators
including Icecast and various soundcard APIs.
It can perform a broad range of signal processing,
combine streams in various ways, support custom transitions,
generate sound procedurally...
and all this can be assembled as you wish.
Input files can be accessed remotely, or even be synthesized on the fly
using external scripts such as speech synthesis.
Finally, interaction with a running liquidsoap instance is possible
via telnet or socket.

Liquidsoap scripts passed on the command line will be evaluated: they shall be
used to define the streaming system to be ran. It is possible to pass multiple
scripts; they will all be ran successively, and definitions from one script can
be used in subsequent ones. A script will be read from standard input if `-` is
given as script filename. Information about scripting liquidsoap is available
on our website: [http://liquidsoap.info/](http://liquidsoap.info/).

If the parameter is not a file it will be treated as an expression which will
be executed. It is a convenient way to test simple one-line scripts. When
running only one-liners, the default is to log messages directly on stdout
rather than to a file.

# OPTIONS

\-
: Read script from standard input.

\--
: Stop parsing the command\-line and pass subsequent items to the script.

\--build-config
: Display liquidsoap's build configuration.

\--cache-only
: Parse, type\-check and save script's cache but do no run it.

\--cache-stdlib
: Generate the standard library cache.

\--debug
: Print debugging log messages.

\--debug-errors
: Debug errors (show stacktrace instead of printing a message).

\--debug-lang
: Debug language implementation.

\--debug-levels
: Debug typing levels.

\--debug-subtyping
: Debug subtyping.

\--disable-deprecated
: Do not load wrappers for deprecated operators.

\--enable-deprecated
: Load wrappers for deprecated operators.

\--interactive
: Start an interactive interpreter.

\--list-deprecated-functions-md
: Documentation of all deprecated functions in markdown.

\--list-extra-functions-md
: Documentation of all extra functions in markdown.

\--list-functions
: List all functions.

\--list-functions-by-category
: List all functions, sorted by category.

\--list-functions-json
: Documentation of all functions in JSON format.

\--list-functions-md
: Documentation of all functions in markdown format.

\--list-plugins
: List all plugins (builtin scripting values, supported formats and protocols).

\--list-portaudio-devices
: List all available portaudio devices

\--list-protocols-md
: Documentation of all protocols in markdown.

\--list-settings
: Display configuration keys in markdown format.

\--no-cache
: Disable cache

\--no-deprecated
: Deprecated: use `--disable-deprecated`

\--no-external-plugins
: Disable external plugins.

\--no-fallible-check
: Ignore fallible sources.

\--no-stdlib
: Do not load stdlib script libraries

\--opam-config
: Print out opam's liquidsoap.config, for internal use.

\--print-json-term
: Parse and output the script as normalized JSON. The JSON format is used internally to format code.

\--profile
: Profile execution.

\--raw-errors
: In normal executions, exceptions raised during the script are translated into user\-friendly errors. Use this option to let the original error surface. This is useful when debugging.

\--safe
: Disable the effects of \--unsafe.

\--stdlib
: Override the location of the standard library.

\--strict
: Execute script code in strict mode, issuing fatal errors instead of warnings in some cases. Currently: unused variables and ignored expressions.

\--unsafe
: Faster startup using unsafe features.

\--version
: Display liquidsoap's version.

\-T
\--disable-telnet
: Disable the telnet server.

\-U
\--disable-unix-socket
: Disable the unix socket.

\-c
\--check
: Parse, type\-check but do not evaluate the script.

\-d
\--daemon
: Run in daemon mode.

\-f
\--force-start
: For advanced dynamic uses: force liquidsoap to start even when no active source is initially defined.

\-h
: Get help about a scripting value: source, operator, builtin or library function, etc.

\-i
: Display inferred types.

\-p
\--parse-only
: Parse script but do not type\-check and run them.

\-q
\--quiet
: Do not print log messages on standard output.

\-r
\--request
: Process a file request and print the metadata.

\-t
\--enable-telnet
: Enable the telnet server.

\-u
\--enable-unix-socket
: Enable the unix socket.

\-v
\--verbose
: Print log messages on standard output.

\-help Display this list of options
\--help Display this list of options

# SEE ALSO

Our website [http://liquidsoap.info/](http://liquidsoap.info/) and the HTML
documentation coming with your distribution of Liquidsoap.

# AUTHOR

[The savonet team](savonet-users@lists.sourceforge.net).
