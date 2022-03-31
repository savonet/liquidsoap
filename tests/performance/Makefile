LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out performance, $(basename $(LIQ))) big-record

test: $(TESTS)

%: %.liq
	@../../src/liquidsoap --no-stdlib ../../libs/stdlib.liq $<

big-record.liq:
	@echo "Generating big-record.liq..."
	@echo "#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq --force-start" > $@
	@echo '%include "performance.liq"' >> $@
	@echo "def sum () =" >> $@
	@echo "r = ()" >> $@
	@for i in `seq 5000`; do echo "let r.a$$i = $$i"; done >> $@
	@echo "n = ref(0)" >> $@
	@for i in `seq 5000`; do echo "n := !n + r.a$$i"; done >> $@
	@echo "end" >> $@
	@echo 'time("sum of fields (big record)", sum)' >> $@
	@echo "exit(0)" >> $@
	@chmod +x $@

perf:
	perf record --call-graph=dwarf -- ../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq erathostenes.liq
	perf report

.PHONY: test
