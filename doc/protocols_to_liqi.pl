#!/usr/local/bin/perl -w

use strict ;
use XML::DOM ;
use Encode 'from_to' ;

my $parser = new XML::DOM::Parser ;
my $doc = $parser->parse (*STDIN) ;
my %cat;

# Select the API items

my $protocols ;
for ($doc->getElementsByTagName("section")) {
  my @l = $_->getElementsByTagName("label") ;
  if (@l && $l[0]->getFirstChild->getData eq "protocols") {
    $protocols = $_ ;
  }
}
die unless $protocols ;

# Translation

sub text_of {
  my $n = shift ;
  return $n->getData ;
}

sub print_protocol {
  my $syntax;
  my $static ;
  my $node = shift ;
  my $label =
    text_of $node->getElementsByTagName("label")->[0]->getFirstChild ;
  my $info =
    text_of $node->getElementsByTagName("info")->[0]->getFirstChild ; 

  for ($node->getElementsByTagName("section")) {
    my @l = $_->getElementsByTagName("label") ;
    if (@l && $l[0]->getFirstChild->getData eq "syntax") {
      $syntax = text_of $_->getElementsByTagName("info")->[0]->getFirstChild ;
    }
    if (@l && $l[0]->getFirstChild->getData eq "static") {
      $static = text_of $_->getElementsByTagName("info")->[0]->getFirstChild ;
    }
  }
  die unless $syntax ;
  die unless $static ;

  $label = lc $label ;
  $label = ucfirst $label ;

  print <<PROTOCOL ;
h5. $label

* Syntax: \@${syntax}@
* Static: \@${static}@

$info

PROTOCOL
}

print <<HEADER ;
title: Protocol reference

h2. Liquidsoap protocol reference



HEADER

for ($protocols->getChildNodes) {
  if ($_->getNodeType == 1) {
    next if $_->getTagName eq "info" ;
    next if $_->getTagName eq "label" ;
    die $_->getTagName unless $_->getTagName eq "section" ;
    print_protocol $_ ;
  }
}
