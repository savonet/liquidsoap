.PHONY: doc upload reference all-subdirs

SUBDIRS = content liqi
DISTFILES = liquidsoap.1 Makefile reference_to_liqi.pl no-ref.txt
DISTDIRS = orig scripts
PERL_DOM = $(shell perl -MXML::DOM -e1 > /dev/null 2>&1 && echo yes)

CONTENT=$(wildcard content/*.txt)
PRE_HTML=$(shell echo $(CONTENT:.txt=.html) | sed -e 's/content\//html\//g') \
     html/reference.html
HTML=html/scripts/index.html
TEX=$(shell echo $(CONTENT:.txt=.tex) | sed -e 's/content\//tex\//g')
PDF=$(TEX:.tex=.pdf)

# PDF is not ready for end-users yet.
doc: html

html: $(HTML)

pdf: liqref.pdf

liqref.pdf: tex/liqref.tex $(TEX)
	@echo Compiling liqref.pdf...
	@cd tex; pdflatex -interaction nonstopmode liqref.tex

html/scripts/index.html: html/scripts/index.txt liqi/liqi
	@echo Generating $(@)...
	@#Has to be called from html to put scripts in the right place
	@cd html && ../liqi/liqi -i ../$(<) -o ../$(@)

# I have to tell make that index.txt will be there
# once the HTML have been generated.
# Also ensures that the index is built last even with a concurrent make run.
html/scripts/index.txt: $(PRE_HTML)

html/%.html: content/%.txt liqi/liqi
	@echo Converting $(<) to $(@)...
	@test -d html || (mkdir -p html/scripts ; cp -rf orig/* html)
	@#Has to be called from html to put scripts in the right place
	@cd html && ../liqi/liqi -i ../$(<) -o ../$(@)

tex/%.tex: content/%.txt liqi/liqi
	@echo Converting $(<) to $(@)...
	@test -d tex || mkdir tex
	@./liqi/liqi --latex -i $(<) -o $(@)

tex/%.pdf: tex/%.tex
	@echo Converting $(<) to $(@)...
	@cd tex; pdflatex ../$(<)

liqi/liqi: all-subdirs
	@# If I don't put anything here, make tries to build liqi/liqi
	@# from liqi/liqi.o by using gcc

../src/liquidsoap:
	@echo "Liquidsoap does not seem to be built. You have to build it before."
	@echo "Go to the toplevel and type 'make'"
	exit 1

content/reference.txt: reference_to_liqi.pl ../src/liquidsoap ../scripts/utils.liq
ifeq ($(PERL_DOM),yes)
	../src/liquidsoap ../scripts/utils.liq --list-plugins-xml | \
	  ./reference_to_liqi.pl > $(@)
else
	@echo "-> You don't seem to have perl XML::DOM module installed."
	@echo "-> Language reference cannot be generated."
	@cp no-ref.txt content/reference.txt
endif

VERSION="`../src/liquidsoap --version | head -n 1`"

test: html
	@echo -e "Testing code snippets with \033[1m$(VERSION)\033[0m"
	@echo ""
	@find html/scripts | grep '\.liq' \
	  | ./scripts/test_snipets "`pwd`/../src/liquidsoap -p"
	@echo ""
	@echo "Testing shell scripts.."
	@echo "Please remember that it does not test liq syntax used inline.."
	@echo ""
	@find html/scripts | grep '\.sh' | ./scripts/test_snipets "sh -n"
	@echo ""
	@echo "Testing perl scripts.."
	@echo ""
	@find html/scripts | grep '\.pl' | ./scripts/test_snipets "perl -c"

doc-install: $(HTML)
	$(INSTALL)  -d $(datadir)/doc/$(DISTDIR)/html
	cp -rf html/* $(datadir)/doc/$(DISTDIR)/html
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1

clean-local:
	rm -rf html $(TEX) content/reference.txt

RCP=rsync -rLtCP --include="*.exe" --exclude="*.swp" --exclude="*.svn" \
	--delete --stats
RHOST=shell.sf.net:/home/groups/s/sa/savonet/htdocs/new/liq-svn
upload: update
	cd html ; $(RCP) . $(RHOST)

top_srcdir=..
include $(top_srcdir)/Makefile.rules
