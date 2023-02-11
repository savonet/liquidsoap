# Harbor input

Liquidsoap is also able to receive a source using icecast or shoutcast source protocol with
the `input.harbor` operator. Using this operator, the running liquidsoap will open
a network socket and wait for an incoming connection.

This operator is very useful to seamlessly add live streams
into your final streams:
you configure the live source client to connect directly to liquidsoap,
and manage the switch to and from the live inside your script.

Additionally, liquidsoap can handle many simultaneous harbor sources on different ports,
with finer-grained authentication schemes that can be particularly useful when used with
source clients designed for the shoutcast servers.

SSL support in harbor can be enabled using of of the following `opam` packages: `ssl`, `osx-secure-transport`.
If enabled using `ssl`, `input.harbor.ssl` will be available. If enabled with `osx-secure-transport`, it will be
`input.harbor.secure_transport`.

## Parameters

The global parameters for harbor can be retrieved using
`liquidsoap --list-settings`. They are:

- `harbor.bind_addr`: IP address on which the HTTP stream receiver should listen. The default is `"0.0.0.0"`. You can use this parameter to restrict connections only to your LAN.
- `harbor.timeout`: Timeout for source connection, in seconds. Defaults to `30.`.
- `harbor.verbose`: Print password used by source clients in logs, for debugging purposes. Defaults to: `false`
- `harbor.reverse_dns`: Perform reverse DNS lookup to get the client's hostname from its IP. Defaults to: `true`
- `harbor.icy_formats`: Content-type (mime) of formats which allow shout (ICY) metadata update. Defaults to: ` ["audio/mpeg"; "audio/aacp"; "audio/aac"; "audio/x-aac"; "audio/wav"; "audio/wave"]`

If SSL support was enabled via `ssl`, you will have the following additional settings:

- `harbor.ssl.certificate`: Path to the SSL certificate.
- `harbor.ssl.private_key`: Path to the SSL private key (openssl only).
- `harbor.ssl.password`: Optional password to unlock the private key.

Obtaining a proper SSL certificate can be tricky. You may want to start with a self-signed certificate first.
You can obtain a free, valid certificate at: [https://letsencrypt.org/](https://letsencrypt.org/)

If SSL support is enable via `osx-secure-transport`, you will have the same settings but named: `harbor.secure_transport.*`.

To create a self-signed certificate for local testing you can use the following one-liner:

```
openssl req -x509 -newkey rsa:4096 -sha256 -nodes -keyout server.key -out server.crt -subj "/CN=localhost" -days 3650
```

You also have per-source parameters. You can retrieve them using the command
`liquidsoap -h input.harbor`. The most important one are:

- `user`, `password`: set a permanent login and password for this harbor source.
- `auth`: Authenticate the user according to a specific function.
- `port`: Use a custom port for this input.
- `icy`: Enable ICY (shoutcast) source connections.
- `id`: The mountpoint registered for the source is also the id of the source.

When using different ports with different harbor inputs, mountpoints are attributed
per-port. Hence, there can be a harbor input with mountpoint `"foo"` on port `1356`
and a harbor input with mountpoint `"foo"` on port `3567`. Additionally, if an harbor
source uses custom port `n` with shoutcast (ICY) source protocol enabled, shoutcast
source clients should set their connection port to `n+1`.

The `auth` function is a function, that takes a record `{user, password, address}` and returns a boolean representing whether the user
should be granted access or not. Typical example can be:

```liquidsoap
def auth(args) =
  # Call an external process to check
  # the credentials:
  # The script will return the string
  # "true" of "false"
  #
  # First call the script. Make sure to apply proper escaping
  # of the arguments to prevent command injection!
  ret = process.read.lines("/path/to/script \
         --user=#{args.user} --password=#{args.password}")
  # Then get the first line of its output
  ret = list.hd(default="",ret)
  # Finally returns the boolean represented
  # by the output (bool_of_string can also
  # be used)
  if ret == "true" then
    true
  else
    false
  end
end
```

In the case of the `ICY` (shoutcast) source protocol, there is no `user` parameter
for the source connection. Thus, the user used will be the `user` parameter passed
to the `input.harbor` source.

When using a custom authentication function, in case of a `ICY` (shoutcast) connection,
the function will receive this value for the username.

## Usage

When using harbor inputs, you first set the required settings, as described above. Then, you define each source using `input.harbor("mountpoint")`. This source is faillible and will become available when a source client is connected.

The unlabeled parameter is the mount point that the source client may connect
to. It should be `"/"` for shoutcast source clients.

The source client may use any of the recognized audio input codec. Hence, when using shoucast source clients, you need to have compiled liquidsoap with mp3 decoding support (`ocaml-mad`)

A sample code can be:

```liquidsoap
settings.harbor.bind_addrs := ["0.0.0.0"]

# Some code...

# This defines a source waiting on mount point
# /test-harbor
live = input.harbor("test-harbor",port=8080,password="xxx")

# This is the final stream.
# Uses the live source as soon as available,
# and don't wait for an end of track, since
# we don't want to cut the beginning of the live
# stream.
#
# You may insert a jingle transition here...
radio = fallback(track_sensitive=false,
                 [live,files])
```
