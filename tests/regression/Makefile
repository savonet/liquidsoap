LIQ = $(sort $(wildcard *.liq))
TESTS = $(filter-out test, $(basename $(LIQ)))
DISTFILES = Makefile $(wildcard *.liq)
top_srcdir = ../..

test: $(TESTS)

include ../Makefile.include
