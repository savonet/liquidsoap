# Download full Liquidsoap and build libraries
git clone https://github.com/savonet/liquidsoap-full.git
cd liquidsoap-full
make init
make update
# We only test with a few packages for now...
cat PACKAGES.default \
    | grep -v ocaml-opus \
    | grep -v ocaml-shine \
    | grep -v ocaml-aacplus \
    | grep -v ocaml-fdkaac \
    > PACKAGES
make -j6 bootstrap
./configure --disable-graphics
make

# Configure current Liquidsoap
LIQ_FULL_DIR=`pwd`
cd ..
LIQ_FULL_DIR=`echo $LIQ_FULL_DIR | sed 's/\\//\\\\\//g'` # replace / with \/
cat liquidsoap-full/liquidsoap/configure-with-options | sed "s/\.\./$LIQ_FULL_DIR/g" > c # replace .. with liquidsoap-full
chmod +x c
./bootstrap
./c

# Compile and run tests
make
make test
