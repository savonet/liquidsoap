# Hacking the build into Travis-CI "C" environment
# See http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

OPAM_PACKAGES='ocamlfind base-bytes camlp4 pcre camomile syslog magic xmlm ocamlnet yojson'

export OPAMYES=1
opam init
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`
opam install -q -y ${OPAM_PACKAGES}

# Download full Liquidsoap
git clone https://github.com/savonet/liquidsoap-full.git
cd liquidsoap-full
make init
# We only test with a few packages for now...
cat PACKAGES.default \
    | grep -v ocaml-portaudio \
    | grep -v ocaml-gstreamer \
    | grep -v ocaml-opus \
    | grep -v ocaml-shine \
    | grep -v ocaml-aacplus \
    | grep -v ocaml-fdkaac \
    > PACKAGES

# compile & run tests
./bootstrap
./configure --disable-graphics
make
