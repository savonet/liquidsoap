## Introduction

You can use external programs in liquidsoap to decode audio files.

## Basic operators

External decoders are registered using decoder.add`.

```liquidsoap
decoder.add(name="my_decoder",description="My custom decoder",
            file_extensions=["foo"], decoder)
```

- `file_extensions` is a list of file extensions that the decoder can handle.
- `decoder` is a function used to return the decoded file.
- You can also use the `mimes` argument to file files based on their mime-type.

The `decoder` function has a similar signature as protocol resolution functions. This is because
file decoding happen as part of the protocol resolution. It takes a `rlog` function, a `maxtime`
maximum execution time stamp and an input file and returns a decoded URI.

Decoded URI can be any url to pass down to the protocol resolution pipeline. Most of the time, it should be
a decoded file but it could also be a `annotate` uri if you wish to also pass down decoded metadata along with
the decoded file.

```{.liquidsoap include="decoder-openmpt.liq"}

```
