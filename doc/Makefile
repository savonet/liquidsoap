
DISTMANUAL := chap_Liquidsoap Liquidsoap Installation LiqQuickstart LiqConcepts LiqScript LiqSettings LiqExamples chap_Advanced_topics LiqTelnetTuto LiqBlankDetection LiqDistributedEncoding chap_Other_tools Bubble Bottle chap_Reference
DISTWIKI := $(shell echo $(DISTMANUAL) | sed "s/chap_[^ ]*//g")

DISTFILES = liquidtts.1 liquidsoap.1 Makefile reference_to_html.pl $(DISTWIKI:=.wiki) reference_to_wiki.pl wiki2tex

top_srcdir=..
include $(top_srcdir)/Makefile.rules

MANUAL := $(DISTMANUAL) LiqReference
WIKI := $(shell echo $(MANUAL) | sed "s/chap_[^ ]* //g")

manual.pdf: manual.tex
	pdflatex manual.tex
	pdflatex manual.tex

manual.tex: $(WIKI:=.wiki) wiki2tex Makefile
	echo "\documentclass[10pt]{book}" > $@
	echo "\usepackage[english]{babel}" >> $@
	echo "\usepackage{a4wide}" >> $@
	echo "\usepackage{fancyvrb}" >> $@
	echo "\usepackage{url}" >> $@
	echo "\usepackage{hyperref}" >> $@
	echo "\sloppy" >> $@
	echo "\hypersetup{hyperindex=true, colorlinks=true, linkcolor=blue}" >> $@
	echo "\title{Liquidsoap manual}" >> $@
	echo "\author{David Baelde and Samuel Mimram}" >> $@
	echo "\begin{document}" >> $@
	echo "\maketitle" >> $@
	echo "\tableofcontents" >> $@
	echo "\newpage" >> $@
	for w in $(MANUAL); do \
	    if echo $$w | grep -q chap; then \
	    echo >> $@; \
	    echo -n "\chapter{" >> $@; \
	    echo -n $$w | sed -e "s/chap_//" | sed -e "s/_/ /g" >> $@; \
	    echo "}" >> $@; \
	    else \
	    ./wiki2tex $$w.wiki >> $@; \
	    fi \
	done
	echo "\end{document}" >> $@

# DISTWIKI wiki files are downloaded by the developper and included in the 
# tarball
download_wiki:
	for i in $(WIKI) ; do \
	  if [ ! $$i = "LiqReference" ] ; then \
		wget http://savonet.sf.net/wiki/$$i/raw -O $$i.wiki ; \
	  fi \
	done

# This one is built using the locally built liquidsoap
LiqReference.wiki:
	../src/liquidsoap --list-plugins-xml | ./reference_to_wiki.pl > $@

# Similar standalone HTML version
lang.html: reference_to_html.pl
	../src/liquidsoap --list-plugins-xml | ./reference_to_html.pl > $@

.PHONY: doc-install

doc-local: manual.pdf lang.html

clean-local:
	rm -f lang.html
	rm -f LiqReference.wiki
	rm -f *.aux *.log *.toc *.pdf *.tex *.out

doc-install:
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)
	$(INSTALL_DATA) manual.pdf $(datadir)/doc/$(DISTDIR)
	$(INSTALL_DATA) lang.html $(datadir)/doc/$(DISTDIR)
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1
