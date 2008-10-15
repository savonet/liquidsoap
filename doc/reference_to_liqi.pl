#!/usr/bin/perl -w

use strict ;
use XML::DOM ;
use Encode 'from_to' ;

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
  my $s = $n->getData ;
  from_to($s,"iso-8859-3","utf-8") ;
  return $s ;
}

sub get_info {
  return text_of $_[0]->getElementsByTagName("info")->[0]->getFirstChild ;
}

sub protect {
  my $v = shift;
  # html wrapping
  $v =~ s/http:\/\/([\w\.\\]*)/"http:\/\/$1":http:\/\/$1/g;
  return $v;
}

sub protect_html {
  my $v = shift;
  $v =~ s/"/&quot;/g ;
  $v =~ s/</&lt\;/g;
  $v =~ s/>/&gt\;/g;
  return $v;
}

sub split_type {
  my $type = shift;
  $type =~ /\((.*)\)->(.*)/;
  my ($pars,$return) = ($1,$2);
  my @params = split(", ",$pars);
  my $maxline = 50;
  my $result = "";
  my $current = "(";
  my $first = 1;
  my ($param,$sep);
  foreach $param (@params) {
    if (length ($current . $param) >= $maxline) {
      $result = $result . $current . ",\n";
      $current = " ";
      $first = 1;
    }
    if ($first == 1) { $sep = "";} else { $sep = ", "; }
    $current = $current . $sep . $param;
    $first = 0;
  }
  $current = $current . ")";
  $first = 0;
  my @returns = split("->",$return);
  foreach $return (@returns) {
    if (length ($current . $return) >= $maxline) {
      $result = $result . $current;
      $current = "";
      $first = 1;
    }
    if ($first == 1) { $sep = "\n ->";} else { $sep = "->"; }
    $current = $current . $sep . $return;
    $first = 0;
  }
  $result . $current;
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
  $b{default} = protect($b{default});
  if ($b{default} ne "None") {
    $default = " -- defaults to <code>".protect_html($b{default})."</code>" ;
  }
  $comment = ": $comment" if $comment ne "" ;
  $comment = protect($comment);

  $cat{$category} .= "* <code>".protect_html($name)."</code> (<code>".protect_html($b{type})."</code>$default)$comment\n";

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

  $cat{$category} .= "h5. ${label}\n<pre>" . split_type($type) . "</pre>\n";

  $cat{$category} .= "WARNING: This is only EXPERIMENTAL!\n"
    if grep { "experimental" eq $_ } @flags ;
  $cat{$category} .= "WARNING: This is DEPRECATED!\n"
    if grep { "deprecated" eq $_ } @flags ;

  # Description

  for (grep { $_->getNodeType == 1 && $_->getTagName eq "info" }
         $node->getChildNodes) {
    next if (text_of $_->getChildNodes->[0]) eq "(no doc)" ;
    $cat{$category} .= "\n" . text_of($_->getChildNodes->[0]) . "\n" ;
  }

  # Parameters

  # Remove the non-param sections from the doc
  sub filter {
    return 0 unless ($_->getNodeType == 1 && $_->getTagName eq "section") ;
    my $l = text_of($_->getElementsByTagName("label")->[0]->getFirstChild) ;
    return ($l ne "category" && $l ne "type" && $l ne "flag") ;
  }
  my @params = grep { filter } $node->getChildNodes ;

  $cat{$category} .= "\n";
  print_param ($_,$category) for (@params) ;
  $cat{$category} .= "\n";

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

print <<HEADER ;
title: Language reference

h3. Liquidsoap scripting language reference

h4. Categories

The **Source / ...** categories contain all functions that return sources.
The **Input** functions are those which build elementary sources
(playing files, synthesizing sound, etc.).
The **Output** functions are those which take a source and register it
for being streamed to the outside (file, soundcard, audio server, etc.).
The **Visualization** functions are experimental ones that let you 
visualize in real-time some aspects of the audio stream.
The **Sound Processing** functions are those which basically work on the source 
as a continuous audio stream. They would typically be mixers of streams,
audio effects or analysis.
Finally, **Track Processing** functions are basically all 
others, often having a behaviour that depends on or affects the extra 
information that liquidsoap puts in streams: track limits and metadata.

HEADER

foreach my $key (sort { compare($a,$b) } (keys %cat)) {
my $anchor = $key;
$anchor =~ s/[^\w]//g;
print "* \"$key\":#$anchor\n";
}
print "\n";

foreach my $key (sort { compare($a,$b) } (keys %cat)) {
my $anchor = $key;
$anchor =~ s/[^\w]//g;
  print <<HTML ;

h4\@$anchor. $key
$cat{$key}
HTML
}

