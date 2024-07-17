#!/usr/bin/perl -w

use strict ;
use XML::DOM ;

my $parser = new XML::DOM::Parser ;
my $doc = $parser->parse (*STDIN) ;

my $indent = 0 ;
my @id = () ;
my @context = ("section") ;
sub offset { ' ' x $indent }

# Select the operators

my $operators ;
for ($doc->getElementsByTagName("section")) {
  my @l = $_->getElementsByTagName("label") ;
  if (@l && $l[0]->getFirstChild->getData eq "operators") {
    $operators = $_ ;
  }
}
die unless $operators ;

# Translation

print <<__HTML__ ;
<html>
<head>
 <link rel="stylesheet" title="default" href="css/default_autodoc.css" />
</head>
<body id="ad-body"><div id="main">
__HTML__

sub text_of {
  my $n = shift ;
  return XML::DOM::encodeText ($n->getData, '<&>"') ;
}

sub idify {
  my $s = shift ;
  $s =~ s/^(.)/uc $1/e ;
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
  $b{default} = "" if $b{default} eq "None" ;

  print offset, '<div class="item-head">', "\n" ;
  $indent++ ;
  print offset, '<span class="type">', $b{type},
    '</span> <span class="name">', $name, "</span>\n" ;
  print offset, "<span class=\"item-head-right\">$b{default}</span>\n" ;
  $indent-- ;
  print offset, '</div><div class="item-body">', $comment, "</div>\n" ;
}

sub print_operator {
  my $node = shift ;

  my $label =
    text_of $node->getElementsByTagName("label")->[0]->getFirstChild ;

  print offset, '<div class="section">', "\n" ;
  $indent++ ;

  print offset, '<div class="section-id">',
    idify ($label), "</div>\n" ;

  # Info

  for (grep { $_->getNodeType == 1 && $_->getTagName eq "info" }
       $node->getChildNodes) {
    next if (text_of $_->getChildNodes->[0]) eq "(no doc)" ;
    print offset, '<div class="par">', "\n" ;
    $indent++ ;
    print offset, (text_of $_->getChildNodes->[0]), "\n" ;
    $indent-- ;
    print offset, "</div>\n" ;
  }

  # Params

  my @params = grep { $_->getNodeType == 1 && $_->getTagName eq "section" }
    $node->getChildNodes ;
  goto END_OPERATOR unless @params ; 
  print offset, '<div class="item">', "\n" ;
  $indent++ ;
  print_param $_ for (@params) ;
  $indent-- ;
  print offset, "</div>\n" ;

  # Done

 END_OPERATOR:
  $indent-- ;
  print offset, "</div>\n" ;
}

for ($operators->getChildNodes) {
  if ($_->getNodeType == 1) {
    if ($_->getTagName eq "info") {
      next ; # No interesting info yet ...
      print offset, "<div class=\"par\">\n" ;
      $indent++ ;
      print offset, (text_of $_->getChildNodes->[0]), "\n" ;
      $indent-- ;
      print offset, "</div>\n" ;
      next ;
    }
    next if $_->getTagName eq "label" ;

    die $_->getTagName unless $_->getTagName eq "section" ;
    print_operator $_ ;
  }
}

