# Hacking the build into Travis-CI "C" environment
# See http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

OPAM_PACKAGES='ocamlfind base-bytes camlp4 pcre camomile syslog magic xmlm ocamlnet yojson inotify'

export OPAMYES=1
opam init
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`
opam install -q -y ${OPAM_PACKAGES}

branch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')

# Download full Liquidsoap
git clone https://github.com/savonet/liquidsoap-full.git
cd liquidsoap-full
make init
make update

# Switch to right branch
# TODO: this does not work, we get something like
# "(detached' from '5e10ffb)" as branch...
#cd liquidsoap
#git checkout $branch
#cd ..

# We only test with a few packages for now...
cat PACKAGES.default \
    | grep -v ocaml-portaudio \
    | grep -v ocaml-gstreamer \
    | grep -v ocaml-opus \
    | grep -v ocaml-shine \
    | grep -v ocaml-aacplus \
    | grep -v ocaml-fdkaac \
    | grep -v ocaml-lastfm \
    > PACKAGES

# Compile and run tests
./bootstrap
./configure --disable-graphics
make
make -C liquidsoap/scripts/tests test
