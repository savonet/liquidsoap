all: wserver
	./wserver

wserver: wserver.ml
	ocamlopt.opt -annot -o wserver unix.cmxa -I +threads threads.cmxa str.cmxa -I +cryptokit nums.cmxa cryptokit.cmxa wserver.ml