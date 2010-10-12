#!/usr/bin/perl

print "title: Liquidsoap language settings\n";

while (<STDIN>) {

s/###/h2. /;
s/##/h3. /;
s/#//;
s/Default: (.+)/Default: \@$1\@/;
s/set\(([^)]+)?\)/%%\nset($1)\n%%/;
s/Comments://;
s/^\s+//;

print $_;
}
