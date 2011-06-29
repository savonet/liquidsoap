.PHONY: doc upload reference all-subdirs

SUBDIRS = content liqi
DISTFILES = liquidsoap.1 Makefile reference_to_liqi.pl settings_to_liqi.pl no-ref.txt \
	    template.html snippet.html
DISTDIRS = orig
PERL_DOM = $(shell perl -MXML::DOM -e1 > /dev/null 2>&1 && echo yes)

CONTENT=$(wildcard content/*.txt)
PRE_HTML=$(shell echo $(CONTENT:.txt=.html) | sed -e 's/content\//html\//g') \
     html/reference.html html/settings.html
HTML=html/scripts/index.html
TEX=$(shell echo $(CONTENT:.txt=.tex) | sed -e 's/content\//tex\//g')
PDF=$(TEX:.tex=.pdf)

LIQI = cd html && ../liqi/liqi \
	     --template ../template.html --snippet-template ../snippet.html \
		 --subst rel:
LIQI_DEPS = liqi/liqi template.html snippet.html

# PDF is not ready for end-users yet.
doc: html

html: $(HTML)

pdf: liqref.pdf

liqref.pdf: tex/liqref.tex $(TEX)
	@echo Compiling liqref.pdf...
	@cd tex; pdflatex -interaction nonstopmode liqref.tex

html/scripts/index.html: html/scripts/index.txt $(LIQI_DEPS)
	@echo Generating $(@)...
	@#Has to be called from html to put scripts in the right place
	$(LIQI) --subst rel:../ -i ../$(<) -o ../$(@)

# I have to tell make that index.txt will be there
# once the HTML have been generated.
# Also ensures that the index is built last even with a concurrent make run.
html/scripts/index.txt: $(PRE_HTML)

html/%.html: content/%.txt $(LIQI_DEPS)
	@echo Converting $(<) to $(@)...
	@test -d html || (mkdir -p html/scripts ; cp -rf orig/* html)
	@#Has to be called from html to put scripts in the right place
	@$(LIQI) -i ../$(<) -o ../$(@)

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
	CAML_LD_LIBRARY_PATH=../src:../src/plugins ../src/liquidsoap --dynamic-plugins-dir ../src/plugins \
	  --no-pervasives ../scripts/pervasives.liq --list-plugins-xml | \
	  ./reference_to_liqi.pl > $(@)
else
	@echo "-> You don't seem to have perl XML::DOM module installed."
	@echo "-> Language reference cannot be generated."
	@cp no-ref.txt content/reference.txt
endif

content/settings.txt: settings_to_liqi.pl ../src/liquidsoap
	CAML_LD_LIBRARY_PATH=../src:../src/plugins ../src/liquidsoap --dynamic-plugins-dir ../src/plugins \
	--conf-descr |  ./settings_to_liqi.pl > $(@)

VERSION="`../src/liquidsoap --version | head -n 1`"

test: html
	@echo "Testing code snippets with \033[1m$(VERSION)\033[0m"
	@echo ""
	@find html/scripts | grep '\.liq' | sort \
	  | ./scripts/test_snippets "`pwd`/../src/liquidsoap -p"
	@echo ""
	@echo "Testing shell scripts.."
	@echo "Please remember that it does not test liq syntax used inline.."
	@echo ""
	@find html/scripts | grep '\.sh' | sort | ./scripts/test_snippets "sh -n"
	@echo ""
	@echo "Testing perl scripts.."
	@echo ""
	@find html/scripts | grep '\.pl' | sort | ./scripts/test_snippets "perl -c"

doc-install: $(HTML)
	$(INSTALL)  -d $(datadir)/doc/$(DISTDIR)/html
	cp -rf html/* $(datadir)/doc/$(DISTDIR)/html
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1

clean-local:
	rm -rf html $(TEX) content/reference.txt content/settings.txt

RCP=rsync -rLtCP --include="*.exe" --exclude="*.swp" --exclude="*.svn" \
	--delete --stats
RHOST=shell.sf.net:/home/groups/s/sa/savonet/htdocs/new/liq-svn
upload: update
	cd html ; $(RCP) . $(RHOST)
	ssh $(USER)@shell.sf.net "chgrp -R savonet \
	                            /home/groups/s/sa/savonet/htdocs \
	                            > /dev/null 2>&1"
	ssh $(USER)@shell.sf.net "find /home/groups/s/sa/savonet/htdocs \
	                            -type d -exec chmod 2775 '{}' ';' \
	                            > /dev/null 2>&1"
	ssh $(USER)@shell.sf.net "find /home/groups/s/sa/savonet/htdocs \
	                            -type f -exec chmod  664 '{}' ';' \
	                            > /dev/null 2>&1"

top_srcdir=..
include $(top_srcdir)/Makefile.rules
