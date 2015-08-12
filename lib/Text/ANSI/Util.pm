package Text::ANSI::Util;

# DATE
# VERSION

use 5.010001;
use strict 'subs', 'vars';
use warnings;

BEGIN {
    eval {
        require Text::WideChar::Util;
        Text::WideChar::Util->import(qw(mbswidth mbtrunc));
    };
    warn if $@;
}

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(
                       ta_add_color_resets
                       ta_detect
                       ta_extract_codes
                       ta_highlight
                       ta_highlight_all
                       ta_length
                       ta_length_height
                       ta_mbpad
                       ta_mbsubstr
                       ta_mbswidth
                       ta_mbswidth_height
                       ta_mbtrunc
                       ta_mbwrap
                       ta_pad
                       ta_split_codes
                       ta_split_codes_single
                       ta_strip
                       ta_substr
                       ta_trunc
                       ta_wrap
               );

use Text::ANSI::BaseUtil ();

our $re = $Text::ANSI::BaseUtil::re;
*{$_} = \&{"Text::ANSI::BaseUtil::$_"} for @EXPORT_OK;

1;
# ABSTRACT: Routines for text containing ANSI escape codes

=encoding UTF-8

=head1 SYNOPSIS

 use Text::ANSI::Util qw(
     ta_add_color_resets
     ta_detect ta_highlight ta_highlight_all ta_length ta_mbpad ta_mbsubstr ta_mbswidth
     ta_mbswidth_height ta_mbwrap ta_pad ta_split_codes ta_split_codes_single
     ta_strip ta_wrap ta_substr);

 # detect whether text has ANSI escape codes?
 say ta_detect("red");       # => false
 say ta_detect("\e[31mred"); # => true

 # calculate length of text (excluding the ANSI escape codes)
 say ta_length("red");       # => 3
 say ta_length("\e[31mred"); # => 3

 # calculate visual width of text if printed on terminal (can handle Unicode
 # wide characters and exclude the ANSI escape codes)
 say ta_mbswidth("\e[31mred");  # => 3
 say ta_mbswidth("\e[31m红色"); # => 4

 # ditto, but also return the number of lines
 say ta_mbswidth_height("\e[31mred\n红色"); # => [4, 2]

 # strip ANSI escape codes
 say ta_strip("\e[31mred"); # => "red"

 # split codes (ANSI codes are always on the even positions)
 my @parts = ta_split_codes("\e[31mred"); # => ("", "\e[31m", "red")

 # wrap text to a certain column width, handle ANSI escape codes
 say ta_wrap("....", 40);

 # ditto, but handle wide characters
 say ta_mbwrap(...);

 # pad (left, right, center) text to a certain width
 say ta_pad("foo", 10);                          # => "foo       "
 say ta_pad("foo", 10, "left");                  # => "       foo"
 say ta_pad("foo\nbarbaz\n", 10, "center", "."); # => "...foo....\n..barbaz..\n"

 # ditto, but handle wide characters
 say ta_mbpad(...);

 # truncate text to a certain width while still passing ANSI escape codes
 use Term::ANSIColor;
 my $text = color("red")."red text".color("reset"); # => "\e[31mred text\e[0m"
 say ta_trunc($text, 5);                            # => "\e[31mred t\e[0m"

 # ditto, but handle wide characters
 say ta_mbtrunc(...);

 # highlight the first occurence of some string within text
 say ta_highlight("some text", "ome", "\e[7m\e[31m");

 # ditto, but highlight all occurrences
 say ta_highlight_all(...);

 # get substring
 my $substr = ta_substr("...", $pos, $len);
 # ditto but with wide character support
 my $substr = ta_mbsubstr("...", $pos, $len);

 # return text but with substring replaced with replacement
 say ta_substr("...", $pos, $len, $replacement);
 # ditto but with wide character support
 say ta_mbsubstr("...", $pos, $len, $replacement);


=head1 DESCRIPTION

This module provides routines for dealing with text containing ANSI escape codes
(mainly ANSI color codes).

Current caveats:

=over

=item * All codes are assumed to have zero width

This is true for color codes and some other codes, but there are also codes to
alter cursor positions which means they can have negative or undefined width.

=item * Single-character CSI (control sequence introducer) currently ignored

Only C<ESC+[> (two-character CSI) is currently parsed.

BTW, in ASCII terminals, single-character CSI is C<0x9b>. In UTF-8 terminals, it
is C<0xc2, 0x9b> (2 bytes).

=item * Private-mode- and trailing-intermediate character currently not parsed

=item * Only color reset code \e[0m is recognized

For simplicity, currently multiple SGR (select graphic rendition) parameters
inside a single ANSI escape code is not parsed. This means that color reset code
like C<\e[1;0m> or C<\e[31;47;0m> is not recognized, only C<\e[0m> is. I believe
this should not be a problem with most real-world text out there.

=back


=head1 FUNCTIONS

=for BEGIN_BLOCK: pod_nonwide_functions

=head2 ta_detect($text) => BOOL

Return true if C<$text> contains ANSI escape codes, false otherwise.

=head2 ta_length($text) => INT

Count the number of characters in $text, while ignoring ANSI escape codes.
Equivalent to C<< length(ta_strip($text)) >>. See also: ta_mbswidth().

=head2 ta_length_height($text) => [INT, INT]

Like ta_length(), but also gives height (number of lines). For example, C<<
ta_length_height("foobar\nb\n") >> gives [6, 3].

=head2 ta_strip($text) => STR

Strip ANSI escape codes from C<$text>, returning the stripped text.

=head2 ta_extract_codes($text) => STR

This is the opposite of C<ta_strip()>, return only the ANSI codes in C<$text>.

=head2 ta_split_codes($text) => LIST

Split C<$text> to a list containing alternating ANSI escape codes and text. ANSI
escape codes are always on the second element, fourth, and so on. Example:

 ta_split_codes("");              # => ()
 ta_split_codes("a");             # => ("a")
 ta_split_codes("a\e[31m");       # => ("a", "\e[31m")
 ta_split_codes("\e[31ma");       # => ("", "\e[31m", "a")
 ta_split_codes("\e[31ma\e[0m");  # => ("", "\e[31m", "a", "\e[0m")
 ta_split_codes("\e[31ma\e[0mb"); # => ("", "\e[31m", "a", "\e[0m", "b")
 ta_split_codes("\e[31m\e[0mb");  # => ("", "\e[31m\e[0m", "b")

so you can do something like:

 my @parts = ta_split_codes($text);
 while (my ($text, $ansicode) = splice(@parts, 0, 2)) {
     ...
 }

=head2 ta_split_codes_single($text) => LIST

Like C<ta_split_codes()> but each ANSI escape code is split separately, instead
of grouped together. This routine is currently used internally e.g. for
C<ta_mbwrap()> and C<ta_highlight()> to trace color reset/replay codes.

=head2 ta_wrap($text, $width, \%opts) => STR

Like L<Text::WideChar::Util>'s wrap() except handles ANSI escape codes. Perform
color reset at the end of each line and a color replay at the start of
subsequent line so the text is safe for combining in a multicolumn/tabular
layout.

Options:

=over

=item * flindent => STR

First line indent. See Text::WideChar::Util for more details.

=item * slindent => STR

First line indent. See Text::WideChar::Util for more details.

=item * tab_width => INT (default: 8)

First line indent. See Text::WideChar::Util for more details.

=item * pad => BOOL (default: 0)

If set to true, will pad each line to C<$width>. This is convenient if you need
the lines padded, saves calls to ta_pad().

=item * return_stats => BOOL (default: 0)

If set to true, then instead of returning the wrapped string, function will
return C<< [$wrapped, $stats] >> where C<$stats> is a hash containing some
information like C<max_word_width>, C<min_word_width>.

=back

Performance: ~500/s on my Core i5 1.7GHz laptop for a ~1KB of text (with zero to
moderate amount of color codes). As a comparison, Text::WideChar::Util's wrap()
can do about 2000/s.

=head2 ta_add_color_resets(@text) => LIST

Make sure that a color reset command (add C<\e[0m>) to the end of each element
and a replay of all the color codes from the previous element, from the last
color reset) to the start of the next element, and so on. Return the new list.

This makes each element safe to be combined with other array of text into a
single line, e.g. in a multicolumn/tabular layout. An example:

Without color resets:

 my @col1 = split /\n/, "\e[31mred\nmerah\e[0m";
 my @col2 = split /\n/, "\e[32mgreen\e[1m\nhijau tebal\e[0m";

 printf "%s | %s\n", $col1[0], $col2[0];
 printf "%s | %s\n", $col1[1], $col2[1];

the printed output:

 \e[31mred | \e[32mgreen
 merah\e[0m | \e[1mhijau tebal\e[0m

The C<merah> text on the second line will become green because of the effect of
the last color command printed (C<\e[32m>). However, with ta_add_color_resets():

 my @col1 = ta_add_color_resets(split /\n/, "\e[31mred\nmerah\e[0m");
 my @col2 = ta_add_color_resets(split /\n/, "\e[32mgreen\e[1m\nhijau tebal\e[0m");

 printf "%s | %s\n", $col1[0], $col2[0];
 printf "%s | %s\n", $col1[1], $col2[1];

the printed output (C<< <...> >>) marks the code added by ta_add_color_resets():

 \e[31mred<\e[0m> | \e[32mgreen\e[1m<\e[0m>
 <\e[31m>merah\e[0m | <\e[32m\e[1m>hijau tebal\e[0m

All the cells are printed with the intended colors.

=head2 ta_pad($text, $width[, $which[, $padchar[, $truncate]]]) => STR

Return C<$text> padded with C<$padchar> to C<$width> columns. C<$which> is
either "r" or "right" for padding on the right (the default if not specified),
"l" or "left" for padding on the right, or "c" or "center" or "centre" for
left+right padding to center the text.

C<$padchar> is whitespace if not specified. It should be string having the width
of 1 column.

Does *not* handle multiline text; you can split text by C</\r?\n/> yourself.

=head2 ta_trunc($text, $width) => STR

Truncate C<$text> to C<$width> columns while still including all the ANSI escape
codes. This ensures that truncated text still reset colors, etc.

Does *not* handle multiline text; you can split text by C</\r?\n/> yourself.

=head2 ta_highlight($text, $needle, $color) => STR

Highlight the first occurence of C<$needle> in C<$text> with <$color>, taking
care not to mess up existing colors.

C<$needle> can be a string or a Regexp object.

Implementation note: to not mess up colors, we save up all color codes from the
last reset (C<\e[0m>) before inserting the highlight color + highlight text.
Then we issue C<\e[0m> and the saved up color code to return back to the color
state before the highlight is inserted. This is the same technique as described
in ta_add_color_resets().

=head2 ta_highlight_all($text, $needle, $color) => STR

Like ta_highlight(), but highlight all occurences instead of only the first.

=head2 ta_substr($text, $pos, $len[ , $replacement ]) => STR

A bit like Perl's C<substr()>. If C<$replacement> is not specified, will return
the substring. If C<$replacement> is specified, will return $text with the
substring replaced by C<$replacement>.

=for END_BLOCK: pod_nonwide_functions

=for BEGIN_BLOCK: pod_wide_functions

=head2 ta_mbpad($text, $width[, $which[, $padchar[, $truncate]]]) => STR

Like ta_pad() but it uses ta_mbswidth() instead of ta_length(), so it can handle
wide characters.

=head2 ta_mbtrunc($text, $width) => STR

Like ta_trunc() but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters.

=head2 ta_mbswidth($text) => INT

Return visual width of C<$text> (in number of columns) if printed on terminal.
Equivalent to C<< Text::WideChar::Util::mbswidth(ta_strip($text)) >>. This
function can be used e.g. in making sure that your text aligns vertically when
output to the terminal in tabular/table format.

Note that C<ta_mbswidth()> handles multiline text correctly, e.g.: C<<
ta_mbswidth("foo\nbarbaz") >> gives 6 instead of 3-1+8 = 8. It splits the input
text first against C<< /\r?\n/ >>.

=head2 ta_mbswidth_height($text) => [INT, INT]

Like C<ta_mbswidth()>, but also gives height (number of lines). For example, C<<
ta_mbswidth_height("西爪哇\nb\n") >> gives [6, 3].

=head2 ta_mbwrap($text, $width, \%opts) => STR

Like ta_wrap(), but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters.

Performance: ~300/s on my Core i5 1.7GHz laptop for a ~1KB of text (with zero to
moderate amount of color codes). As a comparison, Text::WideChar::Util's
mbwrap() can do about 650/s.

=head2 ta_mbsubstr($text, $pos, $len[ , $replacement ]) => STR

Like ta_substr(), but handles wide characters.

=for END_BLOCK: pod_wide_functions


=head1 FAQ

=head2 How do I truncate string based on number of characters?

You can simply use ta_trunc() even on text containing wide characters.
ta_trunc() uses Perl's length() which works on a per-character basis.

=head2 How do I highlight a string case-insensitively?

You can currently use a regex for the C<$needle> and use the C<i> modifier.
Example:

 use Term::ANSIColor;
 ta_highlight($text, qr/\b(foo)\b/i, color("bold red"));


=head1 SEE ALSO

L<Term::ANSIColor>

L<Text::ANSITable> uses this module. In fact, this module was first created
specifically for Text::ANSITable.

http://en.wikipedia.org/wiki/ANSI_escape_code

=cut
