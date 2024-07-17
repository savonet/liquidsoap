#!/usr/bin/perl -w

use strict ;
use DirHandle ;
use XML::DOM ;

# Approximative regexps for OCaml

my $str  = qr/".*?"/ ;
my $bool = qr/false|true/ ;
my $int  = qr/\d+/ ;
my $list = qr/\[\s*(?:\s*$str\s*;)*\s*$str\s*\]/ ;
my $var  = qr/[a-z\.A-Z0-9_\-]+/ ;
my $cst  = qr/$str|$int|$bool|$list/ ;

# XML output

my $doc = new XML::DOM::Document ;
my $node = $doc ;

# Section management

my ($comment, $comment_dist) ;
my $ignore = 0 ;
sub open_section {
  undef $comment ;

  my $id = $_[0] ;
  $id =~ s:.*/:: ;
  $id =~ s/(.)/uc $1/e ;

  my $e = new XML::DOM::Element ($doc,"section") ;
  $e->setAttribute ("id", ($doc==$node?"Main":$id)) ;
  $node->insertBefore ($e) ;
  $node = $e ;
}
sub close_section {
  if (-1==$#{$node->getChildNodes}) {
    my $old = $node ;
    $node = $node->getParentNode ;
    $node->removeChild ($old) ;
  }else{
    $node = $node->getParentNode ;
  }
}

# Parsing a file

sub parse_file {
  return if $_[0] =~ /savonet|fork|strider/i ;
  return unless $_[0] =~ /\.ml$/ ;
  open F, $_[0] ;

  my $line = "" ;
  my $e ;

  LINE:
  while (defined ($line=<F>)) {
    $comment_dist++ ;

    # Annotations
    if ($line =~ /\(\*\*?\s*(.*?)\s*\*\)/) {
      $_ = $1 ;

      # Sections
      if (/<section\s*(.*?)\s*>/) {
        open_section $1 ;
        next LINE ;
      }

      if (/<\/section\s*(.*?)\s*>/) {
        close_section $1 ;
        next LINE ;
      }

      if (/<ignore>/) {
        $ignore = 1 ;
        next LINE ;
      }

      if (/<info\s*(.*?)>/) {
        my $e = new XML::DOM::Element ($doc,"info") ;
        my $t = new XML::DOM::Text ($doc, $1) ;
        $e->insertBefore ($t) ;
        $node->insertBefore ($e) ;
        next LINE ;
      }

      $comment = $1 ; $comment_dist = 0 ;

    }

    # Param reading
    if ($line =~ /get_(string|int|bool|list)\s*
      (?:~(root:(?:$str|$var)))?\s*
      (?:~(default:(?:$cst|$var|$list)))?\s*
      "([^\"]*)"/xg) {

      if ($ignore) {
        print STDERR "Ignored $4 -- it may be normal\n" ;
        $ignore = 0 ;
        next
      }

      my ($type,$val) = ($1,$4) ;
      my ($e,$f) = ($2,$3) ;
      my ($root, $default) ;
      for ($e,$f) {
        next unless defined $_ ;
        $root = $1 if /root:(.*)/ ;
        $default = $1 if /default:(.*)/ ;
      }

      $comment =~ s/</&lt;/g if defined $comment ;
      $comment =~ s/>/&gt;/g if defined $comment ;
      $e = new XML::DOM::Element ($doc, "setting") ;
      map { $e->setAttribute (@{$_}) }
      ( ["name", $val],
        ["type", $type],
        (defined $root?(["root",$root]):()),
        ((defined $comment) && $comment_dist <=3?(["comment",$comment]):()),
        (defined $default?(["default",$default]):()) ) ;

      $node->insertBefore ($e) ;

      undef $comment ;

    }elsif ($line =~ /Conf\.get_/) {
      print "*** Warning *** $line" ;
    }
  }
  close F ;
}

# Parsing a tree

sub parse_dir ;
sub parse_dir {
  my $d = new DirHandle $_[0] ;
  if (defined $d) {
    open_section $_[0] ;
    my $f = "." ;
    while (defined ($f = $d->read)) {
      next if grep { $_ eq $f } ( ".", "..", "CVS" ) ;
      parse_dir ("$_[0]/$f") || parse_file "$_[0]/$f" ;
    }
    close_section $_[0] ;
    undef $d ;
    return 1 ;
  }
  return 0 ;
}

parse_dir ($ARGV[0] || "src") ;

# Add artificial info for dtools

my $daemon = new XML::DOM::Element ($doc, "setting") ;
$daemon->setAttribute("name","daemon") ;
$daemon->setAttribute("default","false") ;
$daemon->setAttribute("type","bool") ;
$daemon->setAttribute("comment","Run in daemon mode?") ;

my $loglevel = new XML::DOM::Element ($doc, "setting") ;
$loglevel->setAttribute("name","log.level.&lt;label&gt;") ;
$loglevel->setAttribute("type","bool") ;
$loglevel->setAttribute("comment",
			"The higher the level is, the more you see ".
			"in the logs about &lt;label&gt;. ".
			"The default level is <code>log.level</code>.") ;

my $stdout = new XML::DOM::Element ($doc, "setting") ;
$stdout->setAttribute("name","log.stdout") ;
$stdout->setAttribute("type","int") ;
$stdout->setAttribute("default","false") ;
$stdout->setAttribute("comment","Print log to stdout?") ;

$doc->getElementsByTagName("section")->[0]->
  insertBefore($loglevel,
	       $doc->getElementsByTagName("section")->[0]->
	       getChildNodes->[0]) ;
$doc->getElementsByTagName("section")->[0]->
  insertBefore($stdout, $loglevel) ;
$doc->getElementsByTagName("section")->[0]->
  insertBefore($daemon, $stdout) ;

# Backup in XML
# $doc->printToFile ("settings_info.xml") ;

# Now, write a XHTML file
# I don't want to bother with DOM and I like nice identation

my $sec ;
my @sections = grep
  { $sec = $_ ;
    grep { $_->getParentNode == $sec && $_->getTagName eq "setting" }
      $sec->getElementsByTagName ("setting") }
  $doc->getElementsByTagName ("section") ;
my $indent = 0 ;
sub offset { return ' 'x$indent }

print <<__HTML__ ;
<html>
 <head>
  <link rel="stylesheet" type="text/css"
        title="default" href="css/default_autodoc.css" />
 </head>
<body id="ad-body">
 <div id="menu"><div id="menu-head">Sections</div><ul>
__HTML__
$indent = 2 ;

for (@sections) {
  print offset, '<li><a href="#sec-', $_->getAttribute("id"), '">' ;
  print $_->getAttribute("id"), "</a></li>\n" ;
}

print <<__HTML__ ;
 </ul></div>

 <div id="main">
__HTML__
$indent = 2 ;

for $sec (@sections) {

  print offset,
    '<div class="section" id="sec-', $sec->getAttribute("id"), "\">\n" ;
  $indent++ ;

  print offset,
    '<span class="section-id">', $sec->getAttribute("id"), "</span>\n" ;

  for (grep { $_->getParentNode == $sec }
       $sec->getElementsByTagName ("info")) {
    my $t = $_->getFirstChild ;
    print offset, '<div class="par">',
      XML::DOM::encodeText ($t->getData, '<&>"'),
	  "</div>\n"
  }

  for (grep { $_->getParentNode == $sec }
       $sec->getElementsByTagName ("setting")) {
    print offset, '<div class="item">', "\n" ;
    $indent++ ;

    print offset, '<div class="item-head">', "\n" ;
    $indent++ ;

    print offset, '<span class="type">',
      $_->getAttribute("type"), "</span> ",
	($_->getAttribute("root")?
	 '<span class="root">&lt;'.$_->getAttribute("root").'&gt;.</span>'
	 :""),
	   '<span class="name">', $_->getAttribute ("name"),
	     "</span>\n" ;

    print offset, '<span class="item-head-right"><span class="default">',
      $_->getAttribute ("default"), "</span></span>\n"
	if ($_->getAttribute("default")) ;

    $indent-- ;
    print offset, "</div>\n" ; # End of item_head

    print offset, "<div class=\"item-body\"><p>\n" ;
    $indent++ ;

    print offset, '<span class="comment">',
      $_->getAttribute ("comment"), "</span>\n"
	if ($_->getAttribute("comment")) ;

    $indent-- ;
    print offset, "</p></div>\n" ;
    $indent-- ;
    print offset, "</div>\n" ;
  }
  $indent-- ;
  print offset, "</div>\n" ;
}

print <<__HTML__ ;
 </div>
</body>
</html>
__HTML__
