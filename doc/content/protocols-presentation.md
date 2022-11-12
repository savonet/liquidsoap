# Protocols

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
protocol.add("s3",s3_protocol,doc="Fetch files from s3 using the AWS CLI",
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
protocol.add("cue_cut",cue_protocol)
```

This protocol returns 30s of data from the input file, stating at the 10s mark.

Likewise, you can apply a normalization program:

```liquidsoap
def normalization_protocol(~rlog,~maxtime,arg) =
  # "normalize" command here is just an example..
  [process_uri(extname="wav",uri=arg,"normalize $(inpuit)")]
end
protocol.add("normalize",normalization_protoco)
```

Now, you can push requests of the form:

```
normalize:cue_cut:http://www.server.com/file.mp3
```

and the file will be cut and normalized
before being played by liquidsoap.

When defining custom protocols, you should pay attention to two variables:

- `rlog` is the logging function. Messages passed to this function will be registered with the request and can be used to debug any issue
- `maxtime` is the maximum time (in UNIX epoch) that the requests should run. After that time, it should return and be considered timed out. You may want to read from `protocols.liq` to see how to enforce this when calling external processes.
