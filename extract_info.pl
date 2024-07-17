#!/usr/bin/perl -w

use strict ;
use XML::DOM ;

my $parser = new XML::DOM::Parser ;
my $doc = $parser->parse (*STDIN) ;

my $indent = 0 ;
my @id = () ;
my @context = ("section") ;
sub offset { ' ' x $indent }

# Remove the operators

my $operators ;
for ($doc->getElementsByTagName("section")) {
  my @l = $_->getElementsByTagName("label") ;
  if (@l && $l[0]->getFirstChild->getData eq "operators") {
    $operators = $_ ;
  }
}
$operators->getParentNode->removeChild($operators) ;

# Translate the remaining document

sub htmlize ;
sub htmlize {
  my $node = shift ;

  my $type = $node->getNodeType ;

  if ($type == 9) { # This is the document root
    print <<__HTML__ ;
<html>
<head>
 <link rel="stylesheet" title="default" href="css/default_autodoc.css" />
</head>
<body id="ad-body"><div id="main">
__HTML__
    htmlize $node->getChildNodes->[0] ;
    print "</body></html>\n" ;
    return ;
  }

  if ($type == 1) { # We've got an element
    my ($id,$class) = ("","") ;

    if ($node->getTagName eq "section") {
      push @id,
	$node->getElementsByTagName("label")->[0]->getFirstChild->getData ;
      $id = join (".", @id) ;
    }
    $id =~ s/^(.)/uc $1/e ;

    # <div class="???">

    if ($node->getTagName eq "info") {
      return if @context == 2 ;
      $class = "par" ;
      goto CLASS_DONE ;
    }

    if ($node->getTagName eq "label") {
      $class=($context[0] eq "section")?"section-id":"item-head" ;
      goto CLASS_DONE ;
    }

    if ($context[0] eq "section") {
      if (grep { $_->getNodeType == 1
		   && $_->getTagName eq "section" }
	  $node->getChildNodes) { # This is _really_ a section
	$class = "section" ;
      }else{
	$class = "item" ;
      }
    }else{
      $class="item_head" ;
    }

  CLASS_DONE:
    print offset, "<div ",
      ($id?"id=\"sec-$id\" ":""),
	'class="', $class, '">', "\n" ;

    $indent++ ;
    unshift @context, $class ;

    for ($node->getChildNodes) {
      htmlize $_ ;
    }

    shift @context ;
    $indent-- ;;

    print offset, "</div>\n" ;

    if ($node->getTagName eq "section") {
      pop @id ;
    }

    return ;
  }

  if ($type == 3) {
    # Some text
    my $s = XML::DOM::encodeText ($node->getData, '<&>"') ;
    if ($context[1] eq "section" && ($context[0] eq "section-id" ||
				     $context[0] eq "item-head")) {
      $s =~ s/^(.)/uc $1/e ;
    }
    return if $s =~ /^\s*$/ ;
    print offset, "$s\n" ;
    return ;
  }

  die ;
}

htmlize $doc ;
