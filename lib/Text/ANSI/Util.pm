package Text::ANSI::Util;

use 5.010001;
use locale;
use strict;
use utf8;
use warnings;

use List::Util qw(max);
use Text::CharWidth qw(mbswidth);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT    = qw(
                       ta_detect
                       ta_length
                       ta_mbswidth
                       ta_mbswidth_height
                       ta_mbwrap
                       ta_strip
                       ta_wrap
               );

# VERSION

# used to find/strip escape codes from string
our $re       = qr/
                      #\x1b\[ (?: (\d+) ((?:;[^;]+?)*) )? ([\x40-\x7e])
                      # without captures
                      \x1b\[ (?: \d+ (?:;[^;]+?)* )? [\x40-\x7e]
                  /osx;

# used to split into words
our $re_words = qr/
                      (?:
                          \S+ |
                          \x1b\[ (?: \d+ (?:;[^;]+?)*)? [\x40-\x7e]
                      )+

                  |

                      \s+
                  /osx;

sub ta_detect {
    my $text = shift;
    $text =~ $re ? 1:0;
}

sub ta_length {
    my $text = shift;
    length(ta_strip($text));
}

sub ta_strip {
    my $text = shift;
    $text =~ s/$re//og;
    $text;
}

sub ta_mbswidth_height {
    my $text = shift;
    my $num_lines = 0;
    my @lens;
    for my $e (split /(\r?\n)/, ta_strip($text)) {
        if ($e =~ /\n/) {
            $num_lines++;
            next;
        }
        $num_lines = 1 if $num_lines == 0;
        push @lens, mbswidth($e);
    }
    [max(@lens) // 0, $num_lines];
}

# same like _ta_mbswidth, but without handling multiline text
sub _ta_mbswidth0 {
    my $text = shift;
    mbswidth(ta_strip($text));
}

sub ta_mbswidth {
    my $text = shift;
    ta_mbswidth_height($text)->[0];
}

sub _ta_wrap {
    my ($is_mb, $text, $width, $opts) = @_;
    $width //= 80;
    $opts  //= {};

    my @res;
    my @p = $text =~ /($re_words)/g;
    #use Data::Dump; dd \@p;
    my $col = 0;
    my $i = 0;
    while (my $p = shift(@p)) {
        $i++;
        my $is_ws;
        my $w;
        #say "D:col=$col, p=$p";
        if ($p =~ /\A\s/s) {
            $is_ws++;
            $p = " ";
            $w = 1;
        } else {
            if ($is_mb) {
                $w = _ta_mbswidth0($p);
            } else {
                $w = ta_length($p);
            }
        }
        $col += $w;
        #say "D:col=$col";
        if ($col > $width+1) {
            # remove whitespace at the end of prev line
            if (@res && $res[-1] eq ' ') {
                pop @res;
            }

            push @res, "\n";
            if ($is_ws) {
                $col = 0;
            } else {
                push @res, $p;
                $col = $w;
            }
        } else {
            push @res, $p if @p;
        }
    }
    join "", @res;
}

sub ta_wrap {
    _ta_wrap(0, @_);
}

sub ta_mbwrap {
    _ta_wrap(1, @_);
}

1;
# ABSTRACT: Routines for text containing ANSI escape codes

=head1 SYNOPSIS

 use Text::ANSI::Util qw(ta_detect ta_length ta_mbswidth ta_mbswidth_height
                         ta_mbwrap ta_strip ta_wrap);

 # detect whether text has ANSI escape codes?
 say ta_detect("red");         # => false
 say ta_detect("\x1b[31mred"); # => true

 # calculate length of text (excluding the ANSI escape codes)
 say ta_length("red");         # => 3
 say ta_length("\x1b[31mred"); # => 3

 # calculate visual width of text if printed on terminal (can handle Unicode
 # wide characters and exclude the ANSI escape codes)
 say ta_mbswidth("\x1b[31mred"); # => 3
 say ta_mbswidth("\x1b[31m红色"); # => 4

 # ditto, but also return the number of lines
 say ta_mbswidth_height("\x1b[31mred\n红色"); # => [4, 2]

 # strip ANSI escape codes
 say ta_strip("\x1b[31mred"); # => "red"

 # wrap text to a certain column width, handle ANSI escape codes
 say ta_wrap("....", 40);

 # ditto, but handle wide characters
 say ta_mbwrap("....", 40);


=head1 DESCRIPTION

This module provides routines for dealing with text containing ANSI escape codes
(mainly ANSI color codes).

Current caveats:

=over

=item * All codes are assumed to have zero width

This is not true. Color codes are indeed zero width, but there are also codes to
alter cursor positions which means it can have negative or undefined width.

=item * Single-character CSI (control sequence introducer) currently ignored

Only C<ESC+[> (two-character CSI) is currently parsed.

In ASCII terminals, single-character CSI is C<0x9b>. In UTF-8 terminals, it is
C<0xc2, 0x9b> (2 bytes).

=item * Private-mode- and trailing-intermediate character currently not parsed

=back


=head1 FUNCTIONS

=head2 ta_detect($text) => BOOL

Return true if C<$text> contains ANSI escape codes, false otherwise.

=head2 ta_length($text) => INT

Count the number of bytes in $text, while ignoring ANSI escape codes. Equivalent
to C<< length(ta_strip($text) >>. See also: ta_mbswidth().

=head2 ta_mbswidth($text) => INT

Return visual width of C<$text> (in number of columns) if printed on terminal.
Equivalent to C<< Text::CharWidth::mbswidth(ta_strip($text)) >>. This function
can be used e.g. in making sure that your text aligns vertically when output to
the terminal in tabular/table format.

Note: C<mbswidth()> handles C<\0> correctly (regard it as having zero width) but
currently does not handle control characters like C<\n>, C<\t>, C<\b>, C<\r>,
etc well (they are just counted as having -1). So make sure that your text does
not contain those characters.

But at least ta_mbswidth() handles multiline text correctly, e.g.: C<<
ta_mbswidth("foo\nbarbaz") >> gives 6 instead of 3-1+8 = 8. It splits the input
text first against C<< /\r?\n/ >>.

=head2 ta_mbswidth_height($text) => [INT, INT]

Like ta_mbswidth(), but also gives height (number of lines). For example, C<<
ta_mbswidth_height("foobar\nb\n") >> gives [6, 3].

=head2 ta_strip($text) => STR

Strip ANSI escape codes from C<$text>, returning the stripped text.

=head2 ta_wrap($text, $width) => STR

Wrap C<$text> to C<$width> columns.

C<$width> defaults to 80 if not specified.

Note: currently performance is rather abysmal (~ 1200/s on my Core i5-2400
3.1GHz desktop for a ~ 1KB of text), so call this routine sparingly ;-).

=head2 ta_mbwrap($text, $width) => STR

Like ta_wrap(), but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters better.

Note: for text which does not have whitespaces between words, like Chinese, you
will have to separate the words first (e.g. using L<Lingua::ZH::WordSegmenter>).
The module also currently does not handle whitespace-like characters other than
ASCII 32 (for example, the Chinese dot 。).

Note: currently performance is rather abysmal (~ 1000/s on my Core i5-2400
3.1GHz desktop for a ~ 1KB of text), so call this routine sparingly ;-).


=head1 TODOS


=head1 SEE ALSO

L<Term::ANSIColor>

=cut
