#!/usr/bin/perl -w

use strict ;
use Net::Telnet ;

my $telnet = new Net::Telnet ( Timeout=>10, Errmode=>'die', Port=>1234) ;
$telnet->open('localhost') ;

die "Usage: $0 <command>\n" unless @ARGV ;
$telnet->print($ARGV[0]) ;
my ($output,$end) = $telnet->waitfor('/END$/') ;
print $output;
