Advanced techniques
===================
We shall now see two things that are essential
to using liquidsoap fully: the server which allows you
to control a running instance of liquidsoap,
and the usage of the `init.d/liquidsoap` service script
for running your radios in production in a clean and convenient way.

Protocols
---------
Protocols in liquidsoap are used to resolve requests URIs. The syntax is: `protocol:arguments`,
for instance: `http://www.example.com`, `say:Something to say` etc.

Most protocols are written using the script language. You can look at the file `protocols.liq` for a list
of them.

In particular, the `process:` protocol can use an external command to prepare resolve a request. Here's an example
using the AWS command-line to download a file from S3:

```liquidsoap
def s3_protocol(~rlog,~maxtime,arg) =
  extname = file.extension(leading_dot=false,dir_sep="/",arg)
  [process_uri(extname=extname,"aws s3 cp s3:#{arg} $(output)")]
end
add_protocol("s3",s3_protocol,doc="Fetch files from s3 using the AWS CLI",
             syntax="s3://uri")
```

Each protocol needs to register a handler, here the `s3_protocol` function. This function takes
the protocol arguments and returns a list of new requests or files. Liquidsoap will then call
this function, collect the returned list and keep resolving requests from the list until it finds a
suitable file.

This makes it possible to create your own custom resolution chain, including for instance cue-points. Here's an example:

```liquidsoap
def cue_protocol(~rlog,~maxtime,arg) =
  [process_uri(extname="wav",uri=uri,"ffmpeg -y -i $(input) -af -ss 10 -t 30 $(output)")]
end
add_protocol("cue_cut",cue_protocol)
```

This protocol returns 30s of data from the input file, stating at the 10s mark.

Likewise, you can apply a normalization program:

```liquidsoap
def normalization_protocol(~rlog,~maxtime,arg) =
  # "normalize" command here is just an example..
  [process_uri(extname="wav",uri=arg,"normalize $(inpuit)")]
end
add_protocol("normalize",normalization_protoco)
```

Now, you can push requests of the form:
```
normalize:cue_cut:http://www.server.com/file.mp3
```
and the file will be cut and normalized
before being played by liquidsoap.

When defining custom protocols, you should pay attention to two variables:

* `rlog` is the logging function. Messages passed to this function will be registered with the request and can be used to debug any issue
* `maxtime` is the maximum time (in UNIX epoch) that the requests should run. After that time, it should return and be considered timed out. You may want to read from `protocols.liq` to see how to enforce this when calling external processes.

Interaction with the server
---------------------------
Liquidsoap starts with one or several scripts as its configuration,
and then streams forever if everything goes well.
Once started, you can still interact with it by means of the *server*.
The server allows you to run commands. Some are general and always available,
some belong to a specific operator. For example the `request.queue()` instances register commands to enqueue new requests, the outputs register commands
to start or stop the outputting, display the last ten metadata chunks, etc.

The protocol of the server is a simple human-readable one.
Currently it does not have any kind of authentication and permissions.
It is currently available via two media: TCP and Unix sockets.
The TCP socket provides a simple telnet-like interface, available only on
the local host by default.
The Unix socket interface (*cf.* the `server.socket` setting)
is through some sort of virtual file.
This is more constraining, which allows one to restrict the use of the socket
to some privileged users.

You can find more details on how to configure the server in the
[documentation](help.html#settings) of the settings key `server`,
in particular `server.telnet` for the TCP interface and `server.socket`
for the Unix interface.
Liquidsoap also embeds some [documentation](help.html#server)
about the available server commands.

Now, we shall simply enable the Telnet interface to the server,
by setting `set("server.telnet",true)` or simply passing the `-t` option on
the command-line.
In a [complete case analysis](complete_case.html) we set up a `request.queue()`
instance to play user requests. It had the identifier `"queue"`.
We are now going to interact via the server to push requests into that queue:

```
dbaelde@selassie:~$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.localdomain.
Escape character is '^]'.
request.push /path/to/some/file.ogg
5
END
metadata 5
[...]
END
request.push http://remote/audio.ogg
6
END
trace 6
[...see if the download started/succeeded...]
END
exit
```

Of course, the server isn't very user-friendly.
But it is easy to write scripts to interact with Liquidsoap in that way,
to implement a website or an IRC interface to your radio.
However, this sort of tool is often bound to a specific usage, so we have
not released any of ours. Feel free to
[ask the community](mailto:savonet-users@lists.sf.net) about code that you could re-use.

Using in production
-------------------
The full installation of liquidsoap will typically install
`/etc/liquidsoap`, `/etc/init.d/liquidsoap` and `/var/log/liquidsoap`.
All these are meant for a particular usage of liquidsoap
when running a stable radio.

Your production `.liq` files should go in `/etc/liquidsoap`.
You'll then start/stop them using the init script, *e.g.*
`/etc/init.d/liquidsoap start`.
Your scripts don't need to have the `#!` line,
and liquidsoap will automatically be ran on daemon mode (`-d` option) for them.

You should not override the `log.file.path` setting because a
logrotate configuration is also installed so that log files
in the standard directory are truncated and compressed if they grow too big.

It is not very convenient to detect errors when using the init script.
We advise users to check their scripts after modification (use
`liquidsoap --check /etc/liquidsoap/script.liq`)
before effectively restarting the daemon.


