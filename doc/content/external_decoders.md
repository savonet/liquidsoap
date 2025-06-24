## Introduction

You can use external programs in liquidsoap to decode audio files. The program must be able to
output WAV data to its standard output (`stdout`) and, possibly, read encoded data from its
standard input.

Please note that this feature is not available under Windows.

## Basic operators

External decoders are registered using the `decoder.add` and `decoder.oblivious.add` operators.
They are invoked the following way:

### `decoder.add`

```liquidsoap
decoder.add(name="my_decoder",description="My custom decoder",
            test,decoder)
```

`decoder.add` is used for external decoders that can read the encoded data from their standard
input (stdin) and write the decoded data as WAV to their standard output (stdout). This operator
is recommended because its estimation of the remaining time is better than the estimation done
by the decoders registered using `decoder.oblivious.add`. The important parameters are:

- `test` is a function used to determine if the file should be decoded by the decoder. Returned values are:
  - `0`: no decodable audio,
  - `-1`: decodable audio but number of audio channels unknown,
  - `x`: fixed number of decodable audio channels.

- `decoder` is the string containing the shell command to run to execute the decoding process.

### `decoder.oblivious.add`

`decoder.oblivious.add` is very similar to `decoder.add`. The main difference is that the
decoding program reads encoded data directly from the local files and not its standard input.
Decoders registered using this operator do not have a reliable estimation of the remaining
time. You should use `decoder.oblivious.add` only if your decoding program is not able
to read the encoded data from its standard input.

```liquidsoap
decoder.oblivious.add(name="my_decoder",description="My custom decoder",
                      buffer=5., test,decoder)
```

`decoder.add` is used for external decoders that can read the encoded data from their standard
input (stdin) and write the decoded data as WAV to their standard output (stdout). This operator
is recommended because its estimation of the remaining time is better than the estimation done
by the decoders registered using `decoder.oblivious.add`. The important parameters are:

- `test` is a function used to determine if the file should be decoded by the decoder. Returned values are:
  - `0`: no decodable audio,
  - `-1`: decodable audio but number of audio channels unknown,
  - `x`: fixed number of decodable audio channels.

- `decoder` is a function that receives the name of the file that should be decoded and returns a string containing the shell command to run to execute the decoding process.

### `decoder.metadata.add`

You may also register new metadata resolvers using the `decoder.metadata.add` operator. It is invoked the
following way: `decoder.metadata.add(format,resolver)`, where:

- `format` is the name of the resolved format. It is only informative.
- `resolver` is a function `f` that returns a list of metadata of the form: `(label, value)`. It is invoked the following way: `f(format=name,file)`, where:
  - `format` contains the name of the format, as returned by the decoder that accepted to decode the file. `f` may return immediately if this is not an expected value.
  - `file` is the name of the file to decode.

## Wrappers

On top of the basic operators, wrappers have been written for some common decoders. This includes the `flac` and
`faad` decoders, by default. All the operators are defined in `externals.liq`.

### The FLAC decoder

The flac decoder uses the `flac` command line. It is enabled if the binary can be found in the current `$PATH`.

Its code is the following:

```{.liquidsoap include="decoder-flac.liq"}

```

Additionally, a metadata resolver is registered when the `metaflac` command can be found in the `$PATH`:

```{.liquidsoap include="decoder-metaflac.liq"}

```

### The faad decoder

The faad decoder uses the `faad` program, if found in the `$PATH`.
It can decode AAC and AAC+ audio files. This program does not support
reading encoded data from its standard input so the decoder is
registered using `decoder.oblivious.add`.

Its code is the following:

```{.liquidsoap include="decoder-faad.liq"}

```
