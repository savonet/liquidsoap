#!/usr/bin/perl -w

use strict ;
use XML::DOM ;

my $parser = new XML::DOM::Parser ;
my $doc = $parser->parse (*STDIN) ;
my %cat;

# Select the API items

my $operators ;
for ($doc->getElementsByTagName("section")) {
  my @l = $_->getElementsByTagName("label") ;
  if (@l && $l[0]->getFirstChild->getData eq "scripting values") {
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

sub get_info {
  return text_of $_[0]->getElementsByTagName("info")->[0]->getFirstChild ;
}

sub print_param {
  my $p = shift ;
  my $category = shift ;

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
  # Surround urls with [[ ]]
  $b{default} =~ s/http:\/\/([\w\.\\]*)/[[http:\/\/$1]]/g;
  $b{default} =~ s/"/&quot;/g ;
  if ($b{default} ne "None") {
    $default = " | defaults to ##".$b{default}."##" ;
  }
  $comment = ": $comment" if $comment ne "" ;

  $cat{$category} .= "~- ##$name## (##".$b{type}."##$default)$comment\n";

}

sub print_operator {
  my $node = shift ;
  my $category = "Uncategorized";
  my @flags = ();

  # Extract the name of the value
  my $label =
    text_of $node->getElementsByTagName("label")->[0]->getFirstChild ;

  # Get the doc node subsections
  # %section associates labels to lists of DOM nodes
  my %section = () ;
  for ($node->getChildNodes) {
    next unless $_->getNodeType == 1 and $_->getTagName eq "section" ;
    push
      @{$section{text_of 
                 $_->getElementsByTagName("label")->[0]->getFirstChild}},
      $_ ;
  }

  # Extract its type and category
  my $type = get_info $section{"type"}->[0] ;
  if ($#{$section{"category"}}>=0) {
    $category = get_info $section{"category"}->[0] ;
  }

  # Get the associated flags
  for (@{$section{"flag"}}) {
    push @flags, get_info $_ ;
  }

  return if grep { "hidden" eq $_ } @flags ;

  $label =~ s/</&lt\;/;
  $label =~ s/>/&gt\;/;
  $cat{$category} .= "====\"\"$label\"\"====\n##$type##\n";

  $cat{$category} .= "WARNING: This is only EXPERIMENTAL!\n"
    if grep { "experimental" eq $_ } @flags ;
  $cat{$category} .= "WARNING: This is DEPRECATED!\n"
    if grep { "deprecated" eq $_ } @flags ;

  # Description

  for (grep { $_->getNodeType == 1 && $_->getTagName eq "info" }
         $node->getChildNodes) {
    next if (text_of $_->getChildNodes->[0]) eq "(no doc)" ;
    $cat{$category} .= text_of($_->getChildNodes->[0]) . "\n" ;
  }

  # Parameters

  # Remove the non-param sections from the doc
  sub filter {
    return 0 unless ($_->getNodeType == 1 && $_->getTagName eq "section") ;
    my $l = text_of($_->getElementsByTagName("label")->[0]->getFirstChild) ;
    return ($l ne "category" && $l ne "type" && $l ne "flag") ;
  }
  my @params = grep { filter } $node->getChildNodes ;

  print_param ($_,$category) for (@params) ;

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

# print "===== Liquidsoap language reference =====\n";

# Custom order that puts "Source /..." first
sub compare {
  my ($a,$b) = @_ ;
  if ($a =~ /^Source/) {
    return -1 unless $b =~ /^Source/ ;
    return $a cmp $b ;
  } else {
    return 1 if $b =~ /^Source/ ;
    return $a cmp $b ;
  }
}
foreach my $key (sort { compare($a,$b) } (keys %cat)) {
  print <<WIKI ;

===== $key =====
$cat{$key}
WIKI
}
