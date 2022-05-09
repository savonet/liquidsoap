# Variables

ML  = interactive.ml
MLI = $(shell ls -d $(ML:.ml=.mli) 2> /dev/null)
CMI = $(MLI:.mli=.cmi)
CMO = $(ML:.ml=.cmo)

OCAML_CFLAGS := -package js_of_ocaml -package js_of_ocaml-ppx
OCAML_LFLAGS := -g -linkpkg

# Generic targets

all: liquidsoap.js

clean:
	rm -f liquidsoap.js $(CMO)

# Build JavaScript

liquidsoap.byte: cmo $(c_objs)
	$(V)echo Generate $@...
	$(V)ocamlfind ocamlc -o liquidsoap $(OCAML_CFLAGS) $(OCAML_LFLAGS) $(CMO) $(c_link)

liquidsoap.js: liquidsoap.byte
	$(V)echo Generate $@...
	$(V)js_of_ocaml compile liquidsoap -o $@

# Generic rules

cmi: $(CMI)
cmo: cmi $(CMO)

%.cmo: %.ml
	$(V)echo OCAMLC -c $<
	$(V)ocamlfind ocamlc $(_OCAML_CFLAGS) $(OCAML_CFLAGS) -c $<
