
DISTMANUAL := chap_Liquidsoap Liquidsoap Installation InstallationUbuntu LiqQuickstart CompleteCaseAnalysis LiqAdvanced LiqConcepts LiqScript LiqSettings LiqExamples LiqFAQ chap_Advanced_topics LiqBlankDetection LiqDistributedEncoding chap_Other_tools Bubble Bottle
DISTWIKI := $(shell echo $(DISTMANUAL) | sed "s/chap_[^ ]*//g")

LIQVERSION = $(shell ../src/liquidsoap --version | head -n 1 | cut -d " " -f "-2" | sed -e 's/\.$$//')
MANVERSION = $(shell cat ./VERSION)

DISTFILES = liquidsoap.1 Makefile reference_to_html.pl $(DISTWIKI:=.wiki) reference_to_wiki.pl wiki2tex *.png VERSION

default: manual.pdf

top_srcdir=..
include $(top_srcdir)/Makefile.rules

MANUAL := $(DISTMANUAL) chap_Reference LiqReference
WIKI := $(shell echo $(MANUAL) | sed "s/chap_[^ ]* //g")

manual.pdf: manual.tex
	pdflatex manual.tex
	pdflatex manual.tex

echo=printf "%s\n"
manual.tex: $(WIKI:=.wiki) wiki2tex Makefile
	$(echo) "\documentclass[10pt]{book}" > $@
	$(echo) "\usepackage[english]{babel}" >> $@
	$(echo) "\usepackage{a4wide}" >> $@
	$(echo) "\usepackage{fancyvrb}" >> $@
	$(echo) "\usepackage{url}" >> $@
	$(echo) "\usepackage{hyperref}" >> $@
	$(echo) "\usepackage{graphicx}" >> $@
	$(echo) "\sloppy" >> $@
	$(echo) "\hypersetup{hyperindex=true, colorlinks=true, linkcolor=blue}" >> $@
	$(echo) "\title{$(LIQVERSION) user's guide. \\\\ \small{Built using online documentation available at \url{http://savonet.sf.net} on $(MANVERSION).}}" >> $@
	$(echo) "\author{David Baelde \and Samuel Mimram \and Romain Beauxis \and Vincent Tabard}" >> $@
	$(echo) "\begin{document}" >> $@
	$(echo) "\maketitle" >> $@
	$(echo) "\tableofcontents" >> $@
	$(echo) "\newpage" >> $@
	for w in $(MANUAL); do \
	    if $(echo) $$w | grep -q chap; then \
	    $(echo) >> $@; \
	    $(echo) "\chapter{" >> $@; \
	    $(echo) $$w | sed -e "s/chap_//" | sed -e "s/_/ /g" >> $@; \
	    $(echo) "}" >> $@; \
	    else \
	    ./wiki2tex $$w.wiki >> $@; \
	    fi \
	done
	$(echo) "\end{document}" >> $@

# DISTWIKI wiki files are downloaded by the developper and included in the 
# tarball
download_wiki:
	for i in $(WIKI) ; do \
	  if [ ! $$i = "LiqReference" ] ; then \
		wget http://savonet.sf.net/wiki/$$i/raw -O $$i.wiki ; \
		./wiki2tex -d $$i.wiki ; \
	  fi \
	done
	LC_ALL=C date +"%b, %d %Y" >| ./VERSION

# This one is built using the locally built liquidsoap
LiqReference.wiki: ../src/liquidsoap reference_to_wiki.pl
	../src/liquidsoap --list-plugins-xml | ./reference_to_wiki.pl > $@

# Similar standalone HTML version
lang.html: reference_to_html.pl
	echo Currently broken > $@
	# ../src/liquidsoap --list-plugins-xml | ./reference_to_html.pl > $@

.PHONY: doc-install download_wiki

doc-local: manual.pdf lang.html

clean-local:
	rm -f lang.html
	rm -f LiqReference.wiki
	rm -f *.aux *.log *.toc *.pdf *.tex *.out 

doc-install:
	@if test \! -f manual.pdf ; then \
	  echo ;\
	  echo "*** It seems that you have not built the documentation." ;\
	  echo "*** Build it using 'make doc' if you have the necessary tools," ;\
	  echo "*** or use 'make fakedoc' to skip that." ;\
	  echo "*** Read INSTALL for more details." ;\
	  echo ;\
	fi
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)
	$(INSTALL_DATA) manual.pdf $(datadir)/doc/$(DISTDIR)
	# $(INSTALL_DATA) lang.html $(datadir)/doc/$(DISTDIR)
	$(INSTALL) -d $(mandir)/man1
	$(INSTALL_DATA) liquidsoap.1 $(mandir)/man1
