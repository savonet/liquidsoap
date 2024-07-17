#!/usr/bin/perl -w

use strict ;
use XML::DOM ;

my $parser = new XML::DOM::Parser ;
my $doc = $parser->parse (*STDIN) ;

# Select the API items

my $operators ;
for ($doc->getElementsByTagName("section")) {
  my @l = $_->getElementsByTagName("label") ;
  if (@l && $l[0]->getFirstChild->getData eq "liqScript builtins") {
    $operators = $_ ;
  }
}
die unless $operators ;

# Translation

sub text_of {
  my $n = shift ;
  # return XML::DOM::encodeText ($n->getData, '<&>"') ;
  return $n->getData ;
}

sub idify {
  my $s = shift ;
  # $s =~ s/^(.)/uc $1/e ;
  return $s ;
}

sub print_param {
  my $p = shift ;

  my $name = text_of
    $p->getElementsByTagName("label")->[0]->getFirstChild ;
  my $comment = text_of
    $p->getElementsByTagName("info")->[0]->getFirstChild ;
  $comment = "" if $comment eq "(no doc)" ;

  my %b = () ;
  for ($p->getElementsByTagName("section")) {
    $b{text_of $_->getElementsByTagName("label")->[0]->getFirstChild}
      = text_of $_->getElementsByTagName("info")->[0]->getFirstChild ;
  }
  my $default = "" ;
  $b{default} =~ s/"/&quot;/g ;
  if ($b{default} ne "None") {
    $default = " | defaults to ##$b{default}##" ;
  }
  $comment = ": $comment" if $comment ne "" ;

  print <<WIKI ;
~- ##$name## (##$b{type}##$default)$comment
WIKI
}

sub print_operator {
  my $node = shift ;

  my $label =
    text_of $node->getElementsByTagName("label")->[0]->getFirstChild ;
  my @types =
  grep {
    $_->getNodeType == 1 &&
    $_->getTagName eq "section" &&
    text_of($_->getElementsByTagName("label")->[0]->getFirstChild) eq "type" }
  $node->getChildNodes ;
  my $type =
    text_of $types[0]->getElementsByTagName("info")->[0]->getFirstChild ;

  print <<WIKI ;

=====$label=====
##$type##
WIKI

  # Info

  for (grep { $_->getNodeType == 1 && $_->getTagName eq "info" }
         $node->getChildNodes) {
    next if (text_of $_->getChildNodes->[0]) eq "(no doc)" ;
    print text_of($_->getChildNodes->[0]), "\n" ;
  }

  # Params

  my @params = grep {
    $_->getNodeType == 1 && $_->getTagName eq "section" &&
    text_of($_->getElementsByTagName("label")->[0]->getFirstChild) ne "type" }
    $node->getChildNodes ;
  print_param $_ for (@params) ;

}

for ($operators->getChildNodes) {
  if ($_->getNodeType == 1) {
    if ($_->getTagName eq "info") {
      next ; # No interesting info yet ...
      print text_of($_->getChildNodes->[0]), "\n" ;
      next ;
    }
    next if $_->getTagName eq "label" ;

    die $_->getTagName unless $_->getTagName eq "section" ;
    print_operator $_ ;
  }
}

