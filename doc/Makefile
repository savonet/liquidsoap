.PHONY: doc upload all-subdirs

SUBDIRS = content
DISTFILES = liquidsoap.1 Makefile template.html snippet.html
DISTDIRS = orig

MD=$(wildcard content/*.md) content/protocols.md content/reference.md content/settings.md
HTML=$(addprefix html/,$(notdir $(MD:.md=.html)))
PDF=$(addprefix pdf/,$(notdir $(MD:.md=.pdf)))

doc: md html

md: $(MD)

html: $(HTML)

pdf: $(PDF)

html/%.html: content/%.md template.html
	@echo Converting $(<) to $(@)...
	@test -d html || (mkdir -p html/scripts ; cp -RfL orig/* html)
	@pandoc $< --metadata pagetitle="`basename '$<'`" --template=template.html -o $@

pdf/%.pdf: content/%.md
	@mkdir -p pdf
	@echo Converting $(<) to $(@)...
	@pandoc $< -o $@

content/protocols.md:
	CAML_LD_LIBRARY_PATH=../src:../src/plugins ../src/liquidsoap --no-pervasives ../lib/pervasives.liq --list-protocols-md >> $@

content/reference.md:
	CAML_LD_LIBRARY_PATH=../src:../src/plugins ../src/liquidsoap --no-pervasives ../lib/pervasives.liq --list-functions-md >> $@

content/settings.md: ../src/liquidsoap
	CAML_LD_LIBRARY_PATH=../src:../src/plugins ../src/liquidsoap --no-pervasives ../lib/pervasives.liq --conf-descr > $@

reference.pdf: content/reference.md
	pandoc $< -o $@

VERSION="`../src/liquidsoap --version | head -n 1`"

doc-install:
	$(INSTALL)  -d $(datadir)/doc/$(DISTDIR)/html
	if [ -f html/index.html ] ; then \
	  cp -RfL html/* $(datadir)/doc/$(DISTDIR)/html ; \
        fi
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1

clean-local:
	rm -rf html pdf content/reference.md content/protocols.md content/settings.md

../src/liquidsoap:
	@echo "Liquidsoap does not seem to be built. You have to build it before."
	@echo "Go to the toplevel and type 'make'"
	@exit 1

top_srcdir=..
include $(top_srcdir)/Makefile.rules

############ SPHINX DOC #########################

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build

sphinx-html:
	@$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# %: Makefile md
#	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: md
