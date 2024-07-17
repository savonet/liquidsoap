
SUBDIRS= src examples doc
DISTFILES = CHANGES COPYING AUTHORS TODO INSTALL README \
	bootstrap configure.ac configure \
	Makefile Makefile.defs.in Makefile.rules install-sh \
	extract_info.pl extract_settings_info.pl extract_lang_info.pl

top_srcdir=.
include $(top_srcdir)/Makefile.rules

auto-clean:
	rm -rf configure config.* autom4te.cache src/configure.ml Makefile.defs
tild-clean:
	find . -name '*~' -exec rm \{\} \;

doc-local:
	./extract_settings_info.pl > doc/html/settings.html
	src/liquidsoap --list-plugins | ./extract_info.pl > doc/html/info.html
	src/liquidsoap --list-plugins | ./extract_lang_info.pl > \
		doc/html/lang.html

.PHONY: system-install gentoo-install debian-install finish-configure

finish-configure:
	echo let tts_program = \"$(bindir)/liquidtts\" >> src/configure.ml
	echo let piddir = \"$(localstatedir)/run/liquidsoap\" >> src/configure.ml
	echo let logdir = \"$(localstatedir)/log/liquidsoap\" >> src/configure.ml

.PHONY: doc-install api-doc-install
doc-install: doc
	make -C doc doc-install
	make -C examples doc-install
api-doc-install: doc
	$(INSTALL) -d $(datadir)/doc/$(DISTDIR)/api
	for doc in $(wildcard autodoc/liquidsoap/*.html autodoc/liquidsoap/*.css) ;\
	do $(INSTALL_DATA) $$doc $(datadir)/doc/$(DISTDIR)/api ; \
	done

# User ${user} and group ${group} are expected to exist.
# They are defined in Makefile.defs, written by configure.

install-local:
ifneq ($(DEBIAN),yes)
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} ${localstatedir}/log/liquidsoap
	$(INSTALL_DIRECTORY) -o ${user} -g ${group} ${localstatedir}/run/liquidsoap
endif
	$(INSTALL_DIRECTORY) ${sysconfdir}/liquidsoap
	$(INSTALL_DATA) examples/radio.liq ${sysconfdir}/liquidsoap
	$(INSTALL_DATA) examples/rtp.out.conf ${sysconfdir}/liquidsoap

gentoo-install:
	$(INSTALL_PROGRAM) -D \
		examples/gentoo-liquidsoap ${sysconfdir}/init.d/liquidsoap
	$(if $(W_RTP),$(INSTALL_PROGRAM) -D \
		examples/gentoo-soaptube ${sysconfdir}/init.d/soaptube)

service-install:
	$(INSTALL_PROGRAM) -D \
		examples/service-liquidsoap ${sysconfdir}/init.d/liquidsoap
	$(if $(W_RTP),$(INSTALL_PROGRAM) -D \
		examples/service-soaptube ${sysconfdir}/init.d/soaptube)
