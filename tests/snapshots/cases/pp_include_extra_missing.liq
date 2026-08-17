# An absent `%include_extra` is skipped, which is how the minimal distributions
# build: they ship none of the files `src/libs/stdlib.liq` pulls in this way.
%include_extra "definitely_not_here.liq-inc"

before = 0
before
