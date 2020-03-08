SUBDIRS= src examples doc gui scripts libs
DISTFILES = CHANGES CHANGES.md COPYING README README.md \
	VERSION bootstrap configure.ac configure config.h.in \
	Makefile Makefile.defs.in Makefile.rules install-sh \
        liquidsoap.opam.in liquidsoap.opam
DISTDIRS = m4

top_srcdir=.
include $(top_srcdir)/Makefile.rules

distclean: pre-distclean
	rm -f Makefile.defs
pre-distclean: clean
	rm -rf config.log config.status config.h autom4te.cache \
	       src/configure.ml scripts/liquidsoap.logrotate \
	       gui/liguidsoap liquidsoap.config $(DISTDIR) $(DISTDIR).tar.bz2

test:
	$(MAKE) -C src/test test
	$(MAKE) -C scripts/tests test

# Build liquidsoap as it will be used for building the doc
doc-local: all

.PHONY: finish-configure

finish-configure:
ifneq ($(CUSTOM_PATH),yes)
	@echo let rundir = \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let logdir = \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
	@echo let libs_dir = \"$(datadir)/liquidsoap/$(libs_dir_version)/libs\" >> src/configure.ml
	@echo let bin_dir = \"$(datadir)/liquidsoap/$(libs_dir_version)/bin\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<sysrundir\>\" \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<syslogdir\>\" \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
else
	@echo let rundir = get_dir \"run\" >> src/configure.ml
	@echo let logdir = get_dir \"logs\" >> src/configure.ml
	@echo let libs_dir = get_dir \"libs\" >> src/configure.ml
	@echo let bin_dir = get_dir \".\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<sysrundir\>\" \".\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<syslogdir\>\" \".\" >> src/configure.ml
endif
	@echo let restart = ref false >> src/configure.ml
	@echo let display_types = ref false >> src/configure.ml
	@echo let exe_ext = \"$(EXEEXT)\" >> src/configure.ml
	@echo "let vendor = \
                  Printf.sprintf \"Liquidsoap/%s (%s; OCaml %s)\" \
                     version Sys.os_type Sys.ocaml_version" >> src/configure.ml
	@echo "let () = Printexc.record_backtrace true" >> src/configure.ml
	@echo "let path = \
          let s = try Sys.getenv \"PATH\" with Not_found -> \"\" in \
          bin_dir :: (Str.split (Str.regexp_string \":\") s)" >> src/configure.ml
	@echo Creating scripts/liquidsoap.logrotate
	@cat scripts/liquidsoap.logrotate.in | \
	  sed -e s:@localstatedir@:$(localstatedir): > scripts/liquidsoap.logrotate

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

# user and group are defined in Makefile.defs, written by configure.

install-local: doc-install
ifeq ($(INSTALL_DAEMON),yes)
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 ${localstatedir}/log/liquidsoap
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 ${localstatedir}/run/liquidsoap
endif
	$(INSTALL_DIRECTORY) $(bindir)
	$(INSTALL_DIRECTORY) $(datadir)/liquidsoap/$(libs_dir_version)/bin
	$(INSTALL_DIRECTORY) $(datadir)/liquidsoap/$(libs_dir_version)/libs
	$(INSTALL_PROGRAM) scripts/extract-replaygain $(datadir)/liquidsoap/$(libs_dir_version)/bin
	find libs | grep '\.liq$$' | while read l; do \
	  $(INSTALL_DATA) $$l $(datadir)/liquidsoap/$(libs_dir_version)/libs ; \
	done
	$(INSTALL_DIRECTORY) ${sysconfdir}/liquidsoap
	$(INSTALL_DATA) examples/radio.liq ${sysconfdir}/liquidsoap/radio.liq.example
	$(INSTALL_DIRECTORY) ${sysconfdir}/logrotate.d
	$(INSTALL_DATA) scripts/liquidsoap.logrotate ${sysconfdir}/logrotate.d/liquidsoap
	-$(INSTALL_DIRECTORY) ${bashcompdir}
	-$(INSTALL_DATA) scripts/bash-completion ${bashcompdir}/liquidsoap
	$(INSTALL_DIRECTORY) ${emacsdir}
	$(INSTALL_DATA) scripts/liquidsoap-mode.el ${emacsdir}/
