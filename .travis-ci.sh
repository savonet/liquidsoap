# Hacking the build into Travis-CI "C" environment
# See http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

OPAM_PACKAGES='ocamlfind base-bytes camlp4 pcre syslog magic xmlm ocamlnet yojson'

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
    | grep -v ocaml-ao \
    | grep -v ocaml-portaudio \
    | grep -v ocaml-bjack \
    | grep -v ocaml-gstreamer \
    | grep -v ocaml-shine \
    | grep -v ocaml-aacplus \
    | grep -v ocaml-speex \
    | grep -v ocaml-opus \
    | grep -v ocaml-schroedinger \
    | grep -v ocaml-voaacenc \
    | grep -v ocaml-fdkaac \
    | grep -v ocaml-ladspa \
    | grep -v ocaml-soundtouch \
    | grep -v ocaml-samplerate \
    | grep -v ocaml-gavl \
    | grep -v ocaml-frei0r \
    | grep -v ocaml-dssi \
    | grep -v ocaml-lastfm \
    > PACKAGES

# compile & run tests
./bootstrap && ./configure && make
