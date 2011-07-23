#!/usr/bin/perl

print "title: Liquidsoap language settings\n";

while (<STDIN>) {

s/###/h3. /;
s/##/h4. /;
s/#//;
s/Default: (.+)/Default: \@$1\@/;
s/set\(([^)]+)?\)/%%\nset($1)\n%%/;
s/Comments://;
s/^\s+//;

print $_;
}
