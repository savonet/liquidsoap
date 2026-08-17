# `%ifdef` in value position: the selected branch is a block used as a value,
# not a statement splice. src/libs/playlist.liq relies on this.
s =
%ifdef list.length
  1
%else
  2
%endif

s
