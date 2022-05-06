default: liquidsoap.js

include Makefile

# Disable unsupported libraries

W_OSS=
W_SSL=

# Needed files

ML  = $(liquidsoap_sources)
MLI = $(shell ls -d $(ML:.ml=.mli) 2> /dev/null)
CMI = $(MLI:.mli=.cmi)
CMO = $(ML:.ml=.cmo)

# Build JavaScript

liquidsoap.byte: cmo $(c_objs)
	$(V)echo Generate $@...
	$(V)ocamlfind ocamlc -o liquidsoap $(_OCAML_CFLAGS) $(OCAML_CFLAGS) $(_OCAML_LFLAGS) $(OCAML_LFLAGS) $(CMO) $(c_link)

liquidsoap.js: liquidsoap.byte
	$(V)echo Generate $@...
	$(V)js_of_ocaml compile liquidsoap -o $@

# Generic rules

cmi: $(CMI)
cmo: cmi $(CMO)

%.cmo: %.ml
	$(V)echo OCAMLC -c $<
	$(V)ocamlfind ocamlc $(_OCAML_CFLAGS) $(OCAML_CFLAGS) -c $<

