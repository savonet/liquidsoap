Liquidsoap on Windows.
======================
Liquidsoap for windows is built using [opam-cross](https://github.com/ocaml-cross/opam-cross-windows). The build process is documented in 
our [docker files](https://github.com/savonet/liquidsoap-full/tree/master/docker). `Dockerfile.win32-deps` installs all 
the [mxe](https://mxe.cc/) dependencies and `Dockerfile.win32` produces the actual liquidsoap binary.

You might want to refer to each project, [mxe](https://mxe.cc/) and [opam-cross](https://github.com/ocaml-cross/opam-cross-windows) for more
details about cross-compiling for windows.


