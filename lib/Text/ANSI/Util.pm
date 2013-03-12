package Text::ANSI::Util;

use 5.010001;
use strict;
use warnings;
use locale;

use List::Util qw(max);
use Text::CharWidth qw(mbswidth);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT    = qw(
                       ta_detect
                       ta_length
                       ta_mbswidth
                       ta_mbswidth_height
                       ta_strip
                       ta_mbwrap
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

sub ta_mbwrap {
    my ($text, $width, $opts) = @_;
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
            $w = 0;
            for my $s (split /( \r?\n|\t| )/, $p) {
                #say "D:s=[$s]";
                if ($s eq ' ') {
                    $w++;
                } elsif ($s eq '\t') {
                    # assume \t to be the same as space, for now
                    $w++;
                } elsif ($s eq '') {
                } else {
                    # newline
                    $col = 0;
                }
                $p = " ";
            }
        } else {
            $w = _ta_mbswidth0($p);
        }
        $col += $w;
        say "D:col=$col";
        if ($col > $width) {
            push @res, "\n";
            say "D:3";
            push @res, $p unless $is_ws;
            $col = 0;
        } else {
            push @res, $p;
        }
    }
    join "", @res;
}

1;
# ABSTRACT: Routines for text containing ANSI escape codes

=head1 SYNOPSIS

 use Text::ANSI::Util qw(ta_detect ta_length ta_size ta_strip ta_mbwrap);


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

=head2 ta_mbwrap($text, $width) => STR

Wrap C<$text> to C<$width> columns. It uses ta_mbswidth() instead of
ta_length(), so it can handle wide characters better.

C<$width> defaults to 80 if not specified.

Note: for text which does not have whitespaces between words, like Chinese, you
will have to separate the words first (e.g. using L<Lingua::ZH::WordSegmenter>).

Note: currently performance is quite abysmal (~ 0.01s on my Core i5 1.7GHz for a
~ 1KB of text), so call this routine sparingly ;-).


=head1 TODOS


=head1 SEE ALSO

L<Term::ANSIColor>

=cut
