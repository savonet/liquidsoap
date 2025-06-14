# Script loading

When you run liquidsoap for streaming, the command line has the following form:

```
$ liquidsoap script_or_expr_1 ... script_or_expr_N
```

This allows you to ask liquidsoap to load definition and settings from
some scripts so that the become available when processing the next ones.

For example you can store your passwords by defined the variable `xxx`
in `secret.liq`, and then refer to that variable in your main script
`main.liq`. You would then run `liquidsoap secret.liq main.liq`. If you ever
need to communicate `main.liq` there won't be any risk of divulgating your
password.

When available, the variable `liquidsoap.script.path` contains the path of the current script's
file and `null` otherwise.

## The pervasive script library

In fact, liquidsoap also implicitly loads scripts before those that you specify
on the command-line. These scripts are meant to contain standard utilities.
Liquidsoap finds them in `LIBDIR/liquidsoap/VERSION` where `LIBDIR` depends on
your configuration (it is typically `/usr/local/lib` or `/usr/lib`) and
`VERSION` is the version of liquidsoap (_e.g._ `0.3.8` or `svn`).

Currently, liquidsoap loads `stdlib.liq` from the library directory,
and this file includes some others.
You can add your personal standard library in that directory
if you find it useful.
