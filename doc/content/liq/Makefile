all:

check:
	@for i in *.liq; do \
	echo -n "Testing $$i... "; \
	./liquidsoap --check $$i; \
	echo " done"; \
	done
