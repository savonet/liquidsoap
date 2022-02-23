DISTFILES = \
	CHANGES CHANGES.md COPYING README README.md \
	bootstrap configure.ac configure config.sub config.guess m4 \
	Makefile Makefile.defs.in install-sh \
	liquidsoap.opam $(wildcard libs/*liq) scripts

all:
	$(MAKE) -C src $@

clean:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(MAKE) -C tests $@

doc:
	if [ "$(OS_TYPE)" = "Win32" ]; then \
	  echo "Building documentation is currently not working on windows, skipping..."; \
        else \
	  $(MAKE) -C src $@; \
	  $(MAKE) -C doc $@; \
	fi

configure: configure.ac install-sh
	./bootstrap
	./configure-with-options || ./configure

distclean:
	rm -f Makefile.defs
	rm -rf config.log config.status autom4te.cache src/configure.ml scripts/liquidsoap.logrotate liquidsoap.config $(DISTDIR) $(DISTDIR).tar.bz2

test:
	@$(MAKE) -C src/test
	@$(MAKE) -C tests $@

.PHONY: clean test distclean doc finish-configure

finish-configure:
ifneq ($(CUSTOM_PATH),yes)
	@echo let rundir = \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let logdir = \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
	@echo let liq_libs_dir = \"$(liq_libs_dir)/libs\" >> src/configure.ml
	@echo let bin_dir = \"$(liq_libs_dir)/bin\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<sysrundir\>\" \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	@echo let \(\) = add_subst \"\<syslogdir\>\" \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml
else
	@echo let rundir = get_dir \"run\" >> src/configure.ml
	@echo let logdir = get_dir \"logs\" >> src/configure.ml
	@echo let liq_libs_dir = get_dir \"libs\" >> src/configure.ml
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
doc-install: doc
	$(MAKE) -C doc $@
api-doc-install:
	$(V)echo "Installing developer documentation..."
	$(V)$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/api
	$(V)for doc in $(wildcard autodoc/liquidsoap/*.html autodoc/liquidsoap/*.css) ;\
	do $(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/api ; \
	done

# user and group are defined in Makefile.defs, written by configure.
.PHONY: install dist print-tarball-filename tarball
install: doc-install
	$(MAKE) -C src $@
ifeq ($(INSTALL_DAEMON),yes)
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 ${localstatedir}/log/liquidsoap
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} -m 2775 ${localstatedir}/run/liquidsoap
endif
	$(INSTALL_DIRECTORY) $(bindir)
	$(INSTALL_DIRECTORY) $(liq_libs_dir)/bin
	$(INSTALL_DIRECTORY) $(liq_libs_dir)/libs
	$(INSTALL_PROGRAM) scripts/extract-replaygain $(liq_libs_dir)/bin
	find libs | grep '\.liq$$' | while read l; do \
	  $(INSTALL_DATA) $$l $(liq_libs_dir)/libs ; \
	done
	$(INSTALL_DIRECTORY) ${sysconfdir}/liquidsoap
	$(INSTALL_DIRECTORY) ${sysconfdir}/logrotate.d
	$(INSTALL_DATA) scripts/liquidsoap.logrotate ${sysconfdir}/logrotate.d/liquidsoap
	-$(INSTALL_DIRECTORY) ${bashcompdir}
	-$(INSTALL_DATA) scripts/bash-completion ${bashcompdir}/liquidsoap
	$(INSTALL_DIRECTORY) ${emacsdir}
	$(INSTALL_DATA) scripts/liquidsoap-mode.el ${emacsdir}/

dist:
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	rsync -amRr $(DISTFILES) $(DISTDIR)
	$(MAKE) -C src DISTDIR=../$(DISTDIR) $@
	$(MAKE) -C doc DISTDIR=../$(DISTDIR) $@
	$(MAKE) -C tests DISTDIR=../$(DISTDIR) $@

print-tarball-filename:
	@printf $(DISTDIR).tar.bz2

tarball: dist
	tar cjf $(DISTDIR).tar.bz2 $(DISTDIR)
	rm -rf $(DISTDIR)

-include Makefile.defs
