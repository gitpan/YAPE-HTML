package YAPE::HTML;

use YAPE::HTML::Element;
use Carp;
use strict;
use vars qw( $VERSION %OPEN %EMPTY );

$VERSION = '1.01';


# when tags get added here, update the POD
my @empty = qw( area base br hr img input link meta param );

@OPEN{@empty, qw( dd dt li p )} = ();
sub OPEN { @OPEN{map lc, @_} = () }

@EMPTY{@empty} = ();
sub EMPTY { @OPEN{map lc, @_} = @EMPTY{map lc, @_} = () }


my %pat = (
  # incomplete DTD support -- add to future version
  DTD => qr{ <!DOCTYPE \s+ (\S+) \s+ (\S+) \s+ "([^"]*)" \s+ "([^"]*)" \s* > }x,

  open_start => qr{ < ([a-zA-Z][a-zA-Z0-9.-]*) }x,
  attr => qr{ \s+ ([\w-]+) (?: \s*=\s* ("[^"]*"|'[^']*'|[^\s>]*) )? }x,
  open_end => qr{ \s* (/?) > }x,

  close => qr{ < / \s* ([a-zA-Z][a-zA-Z0-9.-]*) \s* > }x,

  strcomm => qr{
    <!--
    ( 
      [^-]*
      (?:
        (?! -- \s* > )
        - (?: - [^-]* (?: - [^-]+ )* -- )?
        (?: (?= -- \s* > ) | (?! - ) [^-]* )
      )*
    )
    -- \s* >
  }x,

  comment => qr{ <!-- ( [^-]* (?: (?! -- \s* > ) - [^-]* )* ) -- \s* > }x,

  text => qr{ ( [^<]* (?: <+ [^a-zA-Z</!] [^<]* )* ) (?<= . ) }xs,

  in_script => qr{ ( [^<]* (?: <+ (?! / \s* script \s* > ) [^<]* )* ) }ix,
  end_script => qr{ < / \s* (script) \s* > }ix,

  in_xmp => qr{ ( [^<]* (?: <+ (?! / \s* xmp \s* > ) [^<]* )* ) }ix,
  end_xmp => qr{ < / \s* (xmp) \s* > }ix,
);


sub import {
  shift;
  my @obj = qw( tag closetag text comment );
  no strict 'refs';
  for my $class ('YAPE::HTML', @_) {
    (my $file = $class . ".pm") =~ s!::!/!g;
    require $file and $class->import if not $INC{$file};
    if ($class ne 'YAPE::HTML') {
      push @{"${class}::${_}::ISA"}, "${class}::Element" for @obj;
    }
    push @{"${class}::${_}::ISA"}, 'YAPE::HTML::Element' for @obj;
  }
} 


sub new {
  my ($class, $content, $strict) = @_;

  croak "empty content given to YAPE::HTML->new()" if
    not defined $content or length($content) == 0;

  $strict = 0 if $strict and $strict eq -NO_STRICT;

  my $self = bless {
    STRICT => $strict,
    TREE => [],
    TREE_STACK => [],
    TAG_STACK => [],
    CONTENT => $content,
    ERROR => "",
    STATE => "",
  }, $class;
  $self->{CURRENT} = $self->{TREE};

  return $self;
}


sub state { $_[0]{STATE} }
sub error { $_[0]{ERROR} }
sub chunk { substr $_[0]{CONTENT}, 0, $_[1] || 30 }
sub done { $_[0]{STATE} eq 'done' }
sub root { $_[0]{TREE} }
sub top { $_->type eq 'tag' and $_->tag eq 'html' and return $_ for @{ $_[0]{TREE} } }
sub parse { 1 while $_[0]->next }
sub display {
  my $self = shift;
  $self->parse;
  join "", map $_->fullstring(@_), @{ $self->{TREE} };
}


sub next {
  my $self = shift;

  unless (length $self->{CONTENT}) {
    if (@{ $self->{TAG_STACK} }) {
      if ($self->{STRICT}) {
        while (@{ $self->{TAG_STACK} }) {
          my $tag = pop @{ $self->{TAG_STACK} };
          next if exists $OPEN{$tag};
          $self->{ERROR} = "'<$tag>' never closed";
          $self->{STATE} = 'error';
          return;
        }
      }
      else {
        while (@{ $self->{TAG_STACK} }) {
          my $tag = pop @{ $self->{TAG_STACK} };
          my $node = pop @{ $self->{TREE_STACK} };
          $node->[-1]{IMPLICIT} = 1, next if exists $EMPTY{$tag};
          $node->[-1]{CLOSED} = 1;
        }
      }
    }
              
    $self->{STATE} = 'done';
    return;
  }

  if (@{ $self->{TAG_STACK} } and $self->{TAG_STACK}[-1] eq 'script') {
    if ($self->{CONTENT} =~ s/^$pat{end_script}//) {
      $self->{CURRENT} = pop @{ $self->{TREE_STACK} };
      $self->{CURRENT}[-1]{CLOSED} = 1;
      $self->{STATE} = "close(script)";
      pop @{ $self->{TAG_STACK} };
      pop @{ $self->{CURRENT} };
      return YAPE::HTML::closetag->new($1);
    }
    if ($self->{CONTENT} =~ s/^$pat{in_script}//) {
      push @{ $self->{CURRENT} }, $1;
      $self->{STATE} = "text(select)";
      return YAPE::HTML::text->new($1);
    }
    $self->{STATE} = 'error';
    $self->{ERROR} = "in <SCRIPT>, didn't find </SCRIPT>";
    return;
  }

  if (@{ $self->{TAG_STACK} } and $self->{TAG_STACK}[-1] eq 'xmp') {
    if ($self->{CONTENT} =~ s/^$pat{end_xmp}//) {
      $self->{CURRENT} = pop @{ $self->{TREE_STACK} };
      $self->{CURRENT}[-1]{CLOSED} = 1;
      $self->{STATE} = "close(xmp)";
      pop @{ $self->{TAG_STACK} };
      pop @{ $self->{CURRENT} };
      return YAPE::HTML::closetag->new($1);
    }
    if ($self->{CONTENT} =~ s/^$pat{in_xmp}//) {
      push @{ $self->{CURRENT} }, $1;
      $self->{STATE} = "text(xmp)";
      return YAPE::HTML::text->new($1);
    }
    $self->{STATE} = 'error';
    $self->{ERROR} = "in <XMP>, didn't find </XMP>";
    return;
  }

  if ($self->{CONTENT} =~ s/^$pat{DTD}//) {
    # XXX for future version
    return $self->next;
  }

  if ($self->{CONTENT} =~ s/^$pat{open_start}//) {
    my $element = lc $1;
    my $tag = YAPE::HTML::tag->new($element, {}, []);

    $self->{STATE} = "open($element)";
    if (@{$self->{TAG_STACK}}) {
      if ($self->{TAG_STACK}[-1] eq $element and exists $OPEN{$element}) {
        pop @{ $self->{TAG_STACK} };
        $self->{CURRENT} = pop @{ $self->{TREE_STACK} };
      }
    }

    while ($self->{CONTENT} =~ s/^$pat{attr}//) {
      my ($attr,$val) = ($1,$2);
      defined($val) and $val =~ s/^["']// and chop $val;
      $tag->{ATTR}{lc $attr} = $val;
    }

    if ($self->{CONTENT} =~ s/^$pat{open_end}//) {
      push @{ $self->{CURRENT} }, $tag;
      $tag->{IMPLIED} = $1;
      if (not $1 and not exists $EMPTY{$element}) {
        push @{ $self->{TAG_STACK} }, $element;
        push @{ $self->{TREE_STACK} }, $self->{CURRENT};
        $self->{CURRENT} = $tag->{TEXT};
      }
      return $tag;
    }

    $self->{STATE} = 'error';
    $self->{ERROR} = "in <$element, looking for >";
    return;
  }

  if ($self->{CONTENT} =~ s/^$pat{close}//) {
    my ($tag,$last,$node) = lc $1;

    if ($self->{STRICT}) {
      do {
        $last = pop @{ $self->{TAG_STACK} };
        $node = pop @{ $self->{TREE_STACK} };
      } while $last ne $tag and @{$self->{TAG_STACK}} and exists $OPEN{$last};
    }
    else {
      do {
        $last = pop @{ $self->{TAG_STACK} };
        $node = pop @{ $self->{TREE_STACK} };
        $node->[-1]{CLOSED} = 1;
      } while $last ne $tag and @{$self->{TAG_STACK}};
    }

    if ($last ne $tag) {
      $self->{STATE} = 'error';
      $self->{ERROR} = "wanted '</$last>', found '</$tag>'";
      return;
    }

    $self->{STATE} = "close($tag)";
    ($self->{CURRENT} = $node)->[-1]{CLOSED} = 1;
    return YAPE::HTML::closetag->new($1);
  }

  if ($self->{CONTENT} =~ /^<!--/) {
    if ($self->{CONTENT} =~ s/^$pat{$self->{STRICT} ? 'strcomm' : 'comment'}//) {
      my $comment = YAPE::HTML::comment->new($1);
      push @{ $self->{CURRENT} }, $comment;
      $self->{STATE} = 'comment';
      return $comment;
    }
    $self->{ERROR} = 'malformed comment';
    $self->{STATE} = 'error';
    return;
  }

  if ($self->{CONTENT} =~ s/^$pat{text}// and length $1) {
    my $text = YAPE::HTML::text->new($1);
    push @{ $self->{CURRENT} }, $text;
    $self->{STATE} = 'text';
    return $text;
  }

  $self->{ERROR} = "($self->{STATE}) unknown text";
  $self->{STATE} = 'error';
  return;
}


sub extract {
  my $self = shift;
  $self->parse;

  my $tree = $self->{TREE};
  my (%opts,%tags,@rex,@nodes);

  $opts{lc $1} = shift while @_ and lc($_[0]) =~ /^-(text|comment|tag)/;
  while (@_) {
    my $key = shift;
    if (ref $key) { push @rex, [ $key, shift ] }
    else { $tags{lc $key} = [ map lc, @{ shift() } ] }
  }

  @nodes = @{ $tree };

  return sub {
    my $match;
    while (!$match and @nodes) {
      my $n = shift @nodes;
      my $t = $n->type;
      if ($t eq 'tag' and $opts{tag}) { $match = $n }
      elsif ($t eq 'text' and $opts{text}) { $match = $n }
      elsif ($t eq 'comment' and $opts{comment}) { $match = $n }
      elsif ($t eq 'tag' and $tags{$n->{TAG}}) {
        $match = $n if !grep !exists $n->{ATTR}{$_}, @{ $tags{$n->{TAG}} };
      }
      elsif (@rex and $t eq 'tag') {
        for (@rex) {
          $match = $n, last if
            $n->{TAG} =~ $_->[0] and
            !grep !exists $n->{ATTR}{$_}, @{ $_->[1] };
        }
      }
      unshift @nodes, @{ $n->{TEXT} } if $t eq 'tag';
    }
    return $match;
  };
}


sub quote {
  my $str = pop;
  $str =~ s/"/&quot;/g;
  return $str if length($str) and $str !~ /\D/;
  return qq["$str"];
}


1;
 
__END__

=head1 NAME

YAPE::HTML - Yet Another Parser/Extractor for HTML

=head1 SYNOPSIS

  use YAPE::HTML;
  use strict;
  
  my $content = "<html>...</html>";
  my $parser = YAPE::HTML->new($content);
  my ($extor,@fonts,@urls,@headings,@comments);
  
  # here is the tokenizing part
  while (my $chunk = $parser->next) {
    if ($chunk->type eq 'tag' and $chunk->tag eq 'font') {
      if (my $face = $chunk->get_attr('face')) {
        push @fonts, $face;
      }
    }
  }
  
  # here we catch any errors
  unless ($parser->done) {
    die sprintf "bad HTML: %s (%s)",
      $parser->error, $parser->chunk;
  }
  
  # here is the extracting part
  
  # <A> tags with HREF attributes
  # <IMG> tags with SRC attributes
  $extor = $parser->extract(a => ['href'], img => ['src']);
  while (my $chunk = $extor->()) {
    push @urls, $chunk->get_attr(
      $chunk->tag eq 'a' ? 'href' : 'src'
    );
  }
  
  # <H1>, <H2>, ..., <H6> tags
  $extor = $parser->extract(qr/^h[1-6]$/ => []);
  while (my $chunk = $extor->()) {
    push @headings, $chunk;
  }
  
  # all comments
  $extor = $parser->extract(-COMMENT);
  while (my $chunk = $extor->()) {
    push @comments, $chunk;
  }

=head1 C<YAPE> MODULES

The C<YAPE> hierarchy of modules is an attempt at a unified means of parsing
and extracting content.  It attempts to maintain a generic interface, to
promote simplicity and reusability.  The API is powerful, yet simple.  The
modules do tokenization (which can be intercepted) and build trees, so that
extraction of specific nodes is doable.

=head1 DESCRIPTION

This module is yet another parser and tree-builder for HTML documents.  It is
designed to make extraction and modification of HTML documents simplistic.
The API allows for easy custom additions to the document being parsed, and
allows very specific tag, text, and comment extraction.

=head1 USAGE

In addition to the base class, C<YAPE::HTML>, there is the auxiliary class
C<YAPE::HTML::Element> (common to all C<YAPE> base classes) that holds the
individual nodes' classes.  There is documentation for the node classes in
that module's documentation.

HTML elements and their attributes are stored internally as lowercase strings.
For clarification, that means that the tag C<E<lt>A HREF="FooBar.html"E<gt>> is
stored as

  {
    TAG => 'a',
    ATTR => {
      href => 'FooBar.html',
    }
  }

This means that tags will be output in lowercase.  There will be a feature in a
future version to switch output case to capital letters.

=head2 Functions

=over 4

=item * C<YAPE::HTML::EMPTY(@tags)>

Adds to the internal hash of tags which never contain any out-of-tag content.
This hash is C<%YAPE::HTML::EMPTY>, and contains the following tag names:
C<area>, C<base>, C<br>, C<hr>, C<img>, C<input>, C<link>, C<meta>, and C<param>.
Deletion from this hashmust be done manually.  Adding to this hash automatically
adds to the C<%OPEN> hash, described next.

=item * C<YAPE::HTML::OPEN(@tags)>

Adds to the internal hash of tags which do not require a closing tag.  This hash
is C<%YAPE::HTML::OPEN>, and contains the following tag names:  C<area>, C<base>,
C<br>, C<dd>, C<dt>, C<hr>, C<img>, C<input>, C<li>, C<link>, C<meta>, C<p>, and
C<param>.  Deletion from this hash must be done manually.

There is a subtle difference between "empty" and "open" tags.  For example, the
C<E<lt>AREAE<gt>> tag contains a few attributes, but there is no text associated
with it (nor any other tags), and therefore, is "empty"; the C<E<lt>LIE<gt>>, on
the other hand, 

B<It is strongly suggested> that for ease in parsing, any tags that you do not
explicitly close have a C</> at the end of the tag:

  Here's my cat: <img src="cat.jpg" />

=back

=head2 Methods for C<YAPE::HTML>

=over 4

=item * C<use YAPE::HTML;>

=item * C<use YAPE::HTML qw( MyExt::Mod );>

If supplied no arguments, the module is loaded normally, and the node classes
are given the proper inheritence (from C<YAPE::HTML::Element>).  If you supply
a module (or list of modules), C<import> will automatically include them (if
needed) and set up I<their> node classes with the proper inheritence -- that is,
it will append C<MyExt::Mod::Element> and C<YAPE::HTML::Element> to each node
class's C<@ISA> (where C<MyExt::Mod> is the name of the module being used).

=item * C<my $p = YAPE::HTML-E<gt>new($HTML, $strict);>

Creates a C<YAPE::HTML> object, using the contents of the C<$HTML> string as
its HTML to parse.  The optional second argument determines whether this
parser instance will demand strict comment parsing and require all tags to be
closed with a closing tag or a C</> at the end of the tag (C<E<lt>HR /E<gt>>).
Any true value (except for the special string C<-NO_STRICT>) will turn strict
parsing on.  This is off by default.  (This could be considered a bug.)

=item * C<my $text = $p-E<gt>chunk($len);>

Returns the next C<$len> characters in the input string; C<$len> defaults to
30 characters.  This is useful for figuring out why a parsing error occurs.

=item * C<my $done = $p-E<gt>done;>

Returns true if the parser is done with the input string, and false otherwise.

=item * C<my $errstr = $p-E<gt>error;>

Returns the parser error message.

=item * C<my $coderef = $p-E<gt>extract(...);>

Returns a code reference that returns the next object that matches the criteria
given in the arguments.  The arguments are various; all text:

  $p->extract(-TEXT);

all comments:

  $p->extract(-COMMENT);

all tags:

  $p->extract(-TAG);

specific tags:

  $p->extract(b => [], i => [], u => []);

specific tags with specific attributes:

  $p->extract(a => ['href','target']);

regex object to match tags:

  $p->extract(qr/^h[1-6]$/ => []);

regex object with specific attributes:

  $p->extract(qr/^h[1-6]$/ => ['align']);

or any combination of these -- the exception being that the three constants
must appear before any of the tag-attribute pairs:

  $p->extract(
    -COMMENT,            # all comments
    div => ['align'],    # <DIV> with ALIGN attr
    qr/^t[drh]$/ => [],  # <TD> <TR> <TH> tags
  );

=item * C<my $node = $p-E<gt>display(...);>

Returns a string representation of the entire content.  It calls the C<parse>
method in case there is more data that has not yet been parsed.  This calls the
C<fullstring> method on the root nodes.  Check the C<YAPE::HTML::Element> docs
on the arguments to C<fullstring>.

=item * C<my $node = $p-E<gt>next;>

Returns the next token, or C<undef> if there is no valid token.  There will be
an error message (accessible with the C<error> method) if there was a problem in
the parsing.

=item * C<my $node = $p-E<gt>parse;>

Calls C<next> until all the data has been parsed.

=item * C<my $attr = $p-E<gt>quote($string);>

Returns a quoted string, suitable for using as an attribute.  It turns any
embedded C<"> characters into C<&quot;>.  This can also be called as a raw
function:

  my $quoted = YAPE::HTML::quote($string);

=item * C<my $root = $p-E<gt>root;>

Returns an array reference holding the root of the tree structure -- for
documents that contain multiple top-level tags, this will have more than one
element.

=item * C<my $state = $p-E<gt>state;>

Returns the current state of the parser.  It is one of the following values:
C<close(TAG)>, C<comment>, C<done>, C<error>, C<open(TAG)>, C<text>, 
C<text(script)>, or C<text(xmp)>.  The C<open> and C<close> states contain the
name of the element in parentheses (ex. C<open(img)>).  Tag names, as well as the
names of attributes, are converted to lowercase.  The state of C<text(script)>
refers to text found inside an C<E<lt>SCRIPTE<gt>> element, and likewise for
C<text(xmp)>.

=item * C<my $HTMLnode = $p-E<gt>top;>

Returns the first C<E<lt>HTMLE<gt>> node it finds in the tree structure.

=back

=head1 FEATURES

This is a list of special features of C<YAPE::HTML>.

=over 4

=item * On-the-fly cleaning of HTML

If you aren't enforcing strict HTML syntax, then in the act of parsing HTML, if a
tag that I<should> be closed is not closed, it will be flagged for closing.  That
means that input like:

  <b>Foo<i>bar</b>

will appear as:

  <b>Foo<i>bar</i></b>

upon request for output.  In addition, tags that are left dangling open at the
end of an HTML document get closed.  That means:

  <b>Foo<i>bar

will appear as:

  <b>Foo<i>bar</i></b>

=item * Syntax-checking

If strict checking is off, the only error you'll receive from mismatched HTML
tags is a closing tag out-of-place.

On the other hand, if you do enforce strict HTML syntax, you'll be informed of
tags that do not get closed as well (that should be closed).

=back

=head1 TO DO

This is a listing of things to add to future versions of this module.

=head2 API

=over 4

=item * Proper DTD support

Add an object, C<YAPE::HTML::dtd>, for handling a C<E<lt>!DOCTYPEE<gt>> tag.

=item * HTML entity translation (via C<HTML::Entities> no doubt)

Add a flag to the C<fullstring> method of objects, C<-EXPAND>, which will display
C<&...;> HTML escapes as the character representing them.

=item * SSI parsing support

Add an object, C<YAPE::HTML::ssi>, for handling C<E<lt>!--#command E<gt>> tags.

=item * Toggle case of output (lower/upper case)

Add a flag to the C<fullstring> method of objects, C<-UPPER>, which will display
tag and attribute names in uppercase.

=item * Super-strict syntax checking

DTD-like strictness in regards to nesting of elements -- C<E<lt>LIE<gt>> is not
allowed to be outside an C<E<lt>OLE<gt>> or C<E<lt>ULE<gt>> element.

=back

=head2 Internals

=over 4

=item * Make it faster, of course

There's probably some inherent slowness to this method, but it works.  And it
supports the robust C<extract> method.

=item * Combine C<CLOSED> and C<IMPLICIT>

Make three constants, C<CLOSED_NO>, C<CLOSED_YES>, and C<CLOSED_IMPL>.

=back

=head1 BUGS

Following is a list of known or reported bugs.

=over 4

=item * The above features aren't in here yet.  C<;)>

=item * Strict syntax-checking is not on by default.

=item * This documentation might be incomplete.

=item * Probably need some more test cases.

=back

=head1 SUPPORT

Visit C<YAPE>'s web site at F<http://www.pobox.com/~japhy/YAPE/>.

=head1 SEE ALSO

The C<YAPE::HTML::Element> documentation, for information on the node classes.

=head1 AUTHOR

  Jeff "japhy" Pinyan
  CPAN ID: PINYAN
  japhy@pobox.com
  http://www.pobox.com/~japhy/

=cut
