MANUAL := chap_Liquidsoap Liquidsoap Installation LiqQuickstart LiqConcepts LiqScript LiqSettings LiqExamples chap_Advanced_topics LiqBlankDetection LiqDistributedEncoding chap_Other_tools Bubble Bottle chap_Reference LiqReference
WIKI := $(shell echo $(MANUAL) | sed "s/chap_[^ ]* //g")

manual.pdf: manual.tex
	pdflatex manual.tex
	pdflatex manual.tex

manual.tex: $(wildcard *.wiki) wiki2tex Makefile
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

download_wiki:
	for i in $(WIKI) ; do \
	  wget http://savonet.sf.net/wiki/$$i/raw -O $$i.wiki ; \
	done

clean-local:
	rm -f *.aux *.log *.toc *.pdf *.tex

distclean: clean
	rm -f *.wiki

LiqReference.wiki:
	../../src/liquidsoap --list-plugins-xml | ./extract_lang_info.pl > $@

DISTFILES = Makefile wiki2tex $(wildcard *.wiki) manual.pdf

top_srcdir=../..
include $(top_srcdir)/Makefile.rules

.PHONY: doc-install
doc-install:
	$(INSTALL_DATA) manual.pdf $(datadir)/doc/$(DISTDIR)
doc-local: manual.pdf
