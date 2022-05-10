# Files

INCDIRS = . tools lang stream converters

tools = tools/pos.ml tools/stringView.ml tools/strings.ml tools/log.ml tools/utils.ml tools/runtime_error.ml tools/unifier.ml tools/lifecycle.ml tools/doc.ml tools/plug.ml

stream = stream/frame_settings.ml stream/content.ml stream/frame.ml converters/audio_converter.ml

liquidsoap_sources = console.ml $(tools) $(stream) lang/type.ml lang/term.ml
liquidsoap_sources = 

# Variables

OCAMLCOMP = OCAMLC
OCAMLC = ocamlfind ocamlc

V=@

ML  = configure.ml $(addprefix ../,$(liquidsoap_sources)) interactive.ml
# runtime.ml
MLI = $(shell ls -fd $(ML:.ml=.mli) 2> /dev/null)
CMI = $(MLI:.mli=.cmi)
CMO = $(ML:.ml=.cmo)

OCAML_CFLAGS := $(addprefix -I ../,$(INCDIRS)) -package js_of_ocaml -package js_of_ocaml-ppx
# -package sedlex -package dtools -package menhirLib -thread -package pcre -package mm
OCAML_LFLAGS := -g -linkpkg

# Generic targets

all: liquidsoap.js

clean:
	rm -f liquidsoap.js $(CMI) $(CMO)

test:
	@echo $(MLI)

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

%.cmi: %.mli
	$(V)echo $(OCAMLCOMP) -c $<
	$(V)$(OCAMLC) $(OCAML_CFLAGS) -c $<

%.cmo: %.ml
	$(V)echo OCAMLC -c $<
	$(V)$(OCAMLC) $(OCAML_CFLAGS) -c $<
