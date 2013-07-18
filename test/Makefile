all:

server:
	python -m SimpleHTTPServer


wserver: wserver.ml
	ocamlopt.opt -annot -o wserver unix.cmxa -I +threads threads.cmxa str.cmxa -I +cryptokit nums.cmxa cryptokit.cmxa -I +pulseaudio pulseaudio.cmxa wserver.ml

httpserver: httpserver.ml
	ocamlopt.opt -annot -o httpserver unix.cmxa -I +threads threads.cmxa str.cmxa httpserver.ml

lame:
	@if [ ! -d libmp3lame-js ]; then git clone https://github.com/akrennmair/libmp3lame-js.git; fi

upload:
	rsync -avz lame.html wpps:public_html/tmp/

.PHONY: lame
