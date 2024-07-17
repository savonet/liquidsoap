
SUBDIRS= src examples doc gui scripts
DISTFILES = CHANGES COPYING TODO INSTALL README \
	bootstrap configure.ac configure \
	Makefile Makefile.defs.in Makefile.rules install-sh

top_srcdir=.
include $(top_srcdir)/Makefile.rules

debug:
	$(MAKE) _OCAML_CFLAGS=-g -C src clean all

distclean: clean
	rm -rf config.* autom4te.cache src/configure.ml Makefile.defs scripts/liquidsoap.initd scripts/liquidsoap.gentoo.initd scripts/liquidsoap.logrotate scripts/liquidtts gui/liguidsoap
tild-clean:
	find . -name '*~' -exec rm \{\} \;

# Build liquidsoap as it will be used for building the doc
doc-local: all
fakedoc:
	touch doc/lang.html
	touch doc/manual.tex
	touch doc/manual.pdf
	touch doc/LiqReference.wiki

.PHONY: system-install gentoo-install debian-install finish-configure

finish-configure:
	@echo Creating src/configure.ml
	@echo let tts_program = \"$(libdir)/liquidsoap/liquidtts\" >> src/configure.ml
	@echo let rundir = \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let logdir = \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
	@echo let libs_dir = \"$(libdir)/liquidsoap/$(libs_dir_version)\" >> src/configure.ml
	@echo let display_types = ref false >> src/configure.ml
	@echo let \(\) = add_subst \"\<sysrundir\>\" \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<syslogdir\>\" \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
	@echo Creating scripts/liquidsoap.logrotate
	@cat scripts/liquidsoap.logrotate.in | \
	  sed -e s:@localstatedir@:$(localstatedir): > scripts/liquidsoap.logrotate
	@cat scripts/streamliqueur.in | \
	          sed -e s:@bindir@:$(bindir): > scripts/streamliqueur

.PHONY: doc-install api-doc-install
doc-install:
	$(MAKE) -C doc doc-install
	$(MAKE) -C examples doc-install
api-doc-install:
	$(V)echo Installing developer documentation...
	$(V)$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/api
	$(V)for doc in $(wildcard autodoc/liquidsoap/*.html autodoc/liquidsoap/*.css) ;\
	do $(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/api ; \
	done

# User ${user} and group ${group} are expected to exist.
# They are defined in Makefile.defs, written by configure.

install-local: doc-install
ifneq ($(DEBIAN),yes)
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 \
	  ${localstatedir}/log/liquidsoap
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 \
	  ${localstatedir}/run/liquidsoap
endif
	$(INSTALL_DIRECTORY) $(bindir)
	$(INSTALL_PROGRAM) scripts/streamliqueur $(bindir)
	$(INSTALL_DIRECTORY) $(libdir)/liquidsoap/$(libs_dir_version)
	$(INSTALL_PROGRAM) scripts/liquidtts $(libdir)/liquidsoap
	$(INSTALL_DATA) scripts/utils.liq $(libdir)/liquidsoap/$(libs_dir_version)
	$(INSTALL_DIRECTORY) ${sysconfdir}/liquidsoap
	$(INSTALL_DATA) examples/radio.liq \
	  ${sysconfdir}/liquidsoap/radio.liq.example
	$(INSTALL_DIRECTORY) ${sysconfdir}/logrotate.d
	$(INSTALL_DATA) scripts/liquidsoap.logrotate \
	  ${sysconfdir}/logrotate.d/liquidsoap

gentoo-install:
	$(INSTALL_PROGRAM) -D \
		scripts/liquidsoap.gentoo.initd ${sysconfdir}/init.d/liquidsoap

service-install:
	$(INSTALL_PROGRAM) -D \
		scripts/liquidsoap.initd ${sysconfdir}/init.d/liquidsoap
