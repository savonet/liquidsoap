SUBDIRS = content
DISTFILES = \
	liquidsoap.1.md.in liquidsoap.1.md Makefile template.html snippet.html conf.py index.rst language.dtd liquidsoap.xml \
	$(wildcard content/*.md) $(wildcard content/*.md.in)
DISTDIRS = orig
LIQUIDSOAP = ../src/liquidsoap
PANDOC = pandoc --syntax-definition=liquidsoap.xml --highlight=pygments

MD = $(wildcard content/*.md) content/protocols.md content/reference.md content/reference-extras.md content/settings.md
HTML = $(addprefix html/,$(notdir $(MD:.md=.html)))
PDF = $(addprefix pdf/,$(notdir $(MD:.md=.pdf)))

doc: md man html

md: $(MD)

html: $(HTML)

pdf: $(PDF)

man: liquidsoap.1

clean:
	rm -rf html pdf liquidsoap.1 content/reference.md content/reference-extras.md content/protocols.md content/settings.md

liquidsoap.1: liquidsoap.1.md
	@echo "Generating $(@)..."
	@pandoc -s -t man $< -o $@

html/%.html: content/%.md template.html language.dtd
	@echo Converting $(<) to $(@)...
	@test -d html || (mkdir -p html/scripts ; cp -RfL orig/* html)
	@$(PANDOC) $< --metadata pagetitle="`basename '$<'`" --template=template.html -o $@

pdf/%.pdf: content/%.md language.dtd
	@mkdir -p pdf
	@echo Converting $(<) to $(@)...
	@$(PANDOC) $< -o $@

content/protocols.md: $(LIQUIDSOAP)
	PAGER=none $(LIQUIDSOAP) --no-stdlib ../libs/stdlib.liq --list-protocols-md > $@

content/reference.md: $(LIQUIDSOAP)
	PAGER=none $(LIQUIDSOAP) --no-stdlib ../libs/stdlib.liq --list-functions-md > $@

content/reference-extras.md: $(LIQUIDSOAP)
	PAGER=none $(LIQUIDSOAP) --no-stdlib ../libs/stdlib.liq --list-extra-functions-md > $@

content/settings.md: $(LIQUIDSOAP)
	PAGER=none $(LIQUIDSOAP) --no-stdlib ../libs/stdlib.liq --list-settings > $@

VERSION="`$(LIQUIDSOAP) --version | head -n 1`"

doc-install: doc
	$(INSTALL) -d $(datadir)/doc/liquidsoap/html
	if [ -f html/index.html ] ; then \
	  cp -RfL html/* $(datadir)/doc/liquidsoap/html ; \
        fi
	if [ -f liquidsoap.1 ] ; then \
	  $(INSTALL) -d $(mandir)/man1 ; \
	  $(INSTALL_DATA) liquidsoap.1 $(mandir)/man1 ; \
	fi

$(LIQUIDSOAP):
	@echo "Liquidsoap does not seem to be built. You have to build it before."
	@echo "Go to the toplevel and type 'make'"
	@exit 1

language.dtd:
	curl https://raw.githubusercontent.com/jgm/highlighting-kate/master/xml/language.dtd -o $@

dist:
	mkdir $(DISTDIR)/doc
	rsync -amR $(DISTFILES) $(DISTDIR)/doc/

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

.PHONY: md doc upload all-subdirs

include ../Makefile.defs
