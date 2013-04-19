package Text::ANSI::Util;

use 5.010001;
use locale;
use strict;
use utf8;
use warnings;

use List::Util qw(min max);
use Text::CharWidth qw(mbswidth);
use Text::WideChar::Util qw(mbtrunc);

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
                       ta_mbswidth
                       ta_mbswidth_height
                       ta_mbtrunc
                       ta_mbwrap
                       ta_pad
                       ta_split_codes
                       ta_split_codes_single
                       ta_strip
                       ta_trunc
                       ta_wrap
               );

# VERSION

# used to find/strip escape codes from string
our $re       = qr/
                      #\e\[ (?: (\d+) ((?:;[^;]+?)*) )? ([\x40-\x7e])
                      # without captures
                      \e\[ (?: \d+ (?:;[^;]+?)* )? [\x40-\x7e]
                  /osx;

sub ta_detect {
    my $text = shift;
    $text =~ $re ? 1:0;
}

sub ta_length {
    my $text = shift;
    length(ta_strip($text));
}

sub _ta_length_height {
    my ($is_mb, $text) = @_;
    my $num_lines = 0;
    my @lens;
    for my $e (split /(\r?\n)/, ta_strip($text)) {
        if ($e =~ /\n/) {
            $num_lines++;
            next;
        }
        $num_lines = 1 if $num_lines == 0;
        push @lens, $is_mb ? mbswidth($e) : length($e);
    }
    [max(@lens) // 0, $num_lines];
}

sub ta_length_height {
    _ta_length_height(0, @_);
}

sub ta_mbswidth_height {
    _ta_length_height(1, @_);
}

sub ta_strip {
    my $text = shift;
    $text =~ s/$re//go;
    $text;
}

sub ta_extract_codes {
    my $text = shift;
    my $res = "";
    $res .= $1 while $text =~ /($re)/go;
    $res;
}

sub ta_split_codes {
    my $text = shift;
    return split(/((?:$re)+)/o, $text);
}

sub ta_split_codes_single {
    my $text = shift;
    return split(/($re)/o, $text);
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

    # basically similar to Text::WideChar::Util's algorithm. we adjust for
    # dealing with ANSI codes by splitting codes first (to easily do color
    # resets/restarts), then grouping into words and paras, then doing wrapping.

    my @ch = ta_split_codes_single($text);
    my $crcode = ""; # code for color restart to be put at the start of line
    my @terms;
    my @pterms; # store the plaintext ver, but only for ws to check parabreak
    my @termsw; # store width of each term
    my @termsc; # store color restart code
    my $term     = '';
    my $pterm    = '';
    my $was_word = 0;
    my $was_ws   = 0;
    while (my ($pt, $c) = splice(@ch, 0, 2)) {
        #use Data::Dump; print "D:chunk: "; dd [$pt, $c];
        my @s = $pt =~ /(\S+|\s+)/gos;
        my $once = 1 if !@s;
        while ($once || defined(my $s = shift @s)) {
            $term .= $c if $once;
            $s //= "";
            #say "D:s=[$s]  was_ws=$was_ws  was_word=$was_word  \@ch=",~~@ch,"  \@s=",~~@s;

            if ($s =~ /\S/) {
                if ($was_ws) {
                    #say "D:found word, completed previous ws [$term]";
                    push @terms , $term;
                    push @pterms, $pterm;
                    push @termsw, undef;
                    push @termsc, $crcode;
                    # start new word
                    $pterm = ""; $term = "";
                }
                $pterm .= $s;
                $term  .= $s; $term .= $c if defined($c) && !@s;
                if (!@s && !@ch) {
                    #say "D:complete word because this is the last token";
                    push @terms , $term;
                    push @pterms, undef;
                    push @termsw, $is_mb ? mbswidth($pterm) : length($pterm);
                    push @termsc, $crcode;
                }
                $was_ws = 0; $was_word = 1;
            } elsif (length($s)) {
                if ($was_word) {
                    #say "D:found ws, completed previous word [$term]";
                    push @terms , $term;
                    push @pterms, undef;
                    push @termsw, $is_mb ? mbswidth($pterm) : length($pterm);
                    push @termsc, $crcode;
                    # start new ws
                    $pterm = ''; $term = '';
                }
                $pterm .= $s;
                $term  .= $s; $term .= $c if defined($c) && !@s;
                if (!@s && !@ch) {
                    #say "D:complete ws because this is the last token";
                    push @terms , $term;
                    push @pterms, $pterm;
                    push @termsw, undef;
                    push @termsc, $crcode;
                }
                $was_ws = 1; $was_word = 0;
            }

            if (!@s) {
                if (defined($c) && $c =~ /m\z/) {
                    if ($c eq "\e[0m") {
                        #say "D:found color reset, emptying crcode";
                        $crcode = "";
                    } else {
                        #say "D:adding to crcode";
                        $crcode .= $c;
                    }
                }
                last if $once;
            }

        } # splice @s
    } # splice @ch

    use Data::Dump::Color; dd \@terms; dd \@termsc; dd \@pterms; dd \@termsw; return;

    # now we group terms into paras: we find out flindent, slindent, and strip
    # the rest of the whitespaces.
    my @paras;
    my $words  = [];
    my $wordsw = [];
    my $wordsc = [];
    for my $i (0..$#terms) {
        my $pwo;
        {
            push @paras, {words=>$words, wordsw=>$wordsw, };
            $words = [];
        }
    }

    # now we perform wrapping

    my $tw = $opts->{tab_width} // 8;
    die "Please specify a positive tab width" unless $tw > 0;
    my $optfli  = $opts->{flindent};
    my $optfliw = Text::WideChar::Util::_get_indent_width($is_mb, $optfli, $tw) if defined $optfli;
    my $optsli  = $opts->{slindent};
    my $optsliw = Text::WideChar::Util::_get_indent_width($is_mb, $optsli, $tw) if defined $optsli;
    my @res;

    my $paranum = 0;
  PARA:
    while (my ($paratext, $parabreak) = splice @paras, 0, 2) {
        $paranum++;
        my $x = 0;
        my $y = 0;
        my $line_has_word = 0;

        # determine indents
        my ($fli, $sli, $fliw, $sliw);
        if (defined $optfli) {
            $fli  = $optfli;
            $fliw = $optfliw;
        } else {
            # XXX emacs can also treat ' #' as indent, e.g. when wrapping
            # multi-line perl comment.
            my ($ptextl1) = $paratext =~ /\A([^\n]*)/;
            $ptextl1 = ta_strip($ptextl1);
            ($fli) = $ptextl1 =~ /\A([ \t]*)\S/;
            if (defined $fli) {
                $fliw = Text::WideChar::Util::_get_indent_width($is_mb, $fli, $tw);
            } else {
                $fli  = "";
                $fliw = 0;
            }
        }
        if (defined $optsli) {
            $sli  = $optsli;
            $sliw = $optsliw;
        } else {
            my ($ptextl2) = $paratext =~ /\A[^\n]*\n([^\n]*)/;
            $ptextl2 = ta_strip($ptextl2 // "");
            ($sli) = $ptextl2 =~ /\A([ \t+]*)\S/;
            if (defined $sli) {
                $sliw = Text::WideChar::Util::_get_indent_width($is_mb, $sli, $tw);
            } else {
                $sli  = "";
                $sliw = 0;
            }
        }
        die "Subsequent indent must be less than width" if $sliw >= $width;

        #push @res, $clrres if $paranum > 1;
        push @res, $fli;
        $x += $fliw;

        # process each word
        for my $word0 ($paratext =~ /(\S+)/g) {
            my @words;
            my @wordsw;
            while (1) {
                my $wordw = $is_mb ? mbswidth($word0) : length($word0);
                if ($wordw <= $width-$sliw) {
                    push @words , $word0;
                    push @wordsw, $wordw;
                    last;
                }
                # truncate long word
                if ($is_mb) {
                    my $res = mbtrunc($text, $width-$sliw, 1);
                    push @words , $res->[0];
                    push @wordsw, $res->[1];
                    $word0 = substr($word0, length($res->[0]));
                } else {
                    my $w2 = substr($word0, 0, $width-$sliw);
                    push @words , $w2;
                    push @wordsw, $width-$sliw;
                    $word0 = substr($word0, $width-$sliw);
                }
            }

            for my $word (@words) {
                my $wordw = shift @wordsw;
                #say "x=$x word=$word wordw=$wordw line_has_word=$line_has_word width=$width";
                if ($x + ($line_has_word ? 1:0) + $wordw <= $width) {
                    if ($line_has_word) {
                        push @res, " ";
                        $x++;
                    }
                    push @res, $word;
                    $x += $wordw;
                } else {
                    push @res, "\n", $sli, $word;
                    $x = $sliw + $wordw;
                    $y++;
                }
                $line_has_word++;
            }
        }

        if (defined $parabreak) {
            push @res, $parabreak;
        } else {
            push @res, "\n" if $paratext =~ /\n[ \t]*\z/;
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

sub _ta_pad {
    my ($is_mb, $text, $width, $which, $padchar, $is_trunc) = @_;
    if ($which) {
        $which = substr($which, 0, 1);
    } else {
        $which = "r";
    }
    $padchar //= " ";

    my $w = $is_mb ? _ta_mbswidth0($text) : ta_length($text);
    if ($is_trunc && $w > $width) {
        my $res = $is_mb ?
            ta_mbtrunc($text, $width, 1) : ta_trunc($text, $width, 1);
        $text = $res->[0] . ($padchar x ($width-$res->[1]));
    } else {
        if ($which eq 'l') {
            $text = ($padchar x ($width-$w)) . $text;
        } elsif ($which eq 'c') {
            my $n = int(($width-$w)/2);
            $text = ($padchar x $n) . $text . ($padchar x ($width-$w-$n));
        } else {
            $text .= ($padchar x ($width-$w));
        }
    }
    $text;
}

sub ta_pad {
    _ta_pad(0, @_);
}

sub ta_mbpad {
    _ta_pad(1, @_);
}

sub _ta_trunc {
    my ($is_mb, $text, $width, $return_width) = @_;

    my $w = $is_mb ? _ta_mbswidth0($text) : ta_length($text);
    return $text if $w <= $width;
    my @p = ta_split_codes($text);
    my @res;
    my $append = 1; # whether we should add more text
    $w = 0;
    while (my ($t, $ansi) = splice @p, 0, 2) {
        if ($append) {
            my $tw = $is_mb ? mbswidth($t) : length($t);
            if ($w+$tw <= $width) {
                push @res, $t;
                $w += $tw;
                $append = 0 if $w == $width;
            } else {
                my $tres = $is_mb ?
                    mbtrunc($t, $width-$w, 1) :
                        [substr($t, 0, $width-$w), $width-$w];
                push @res, $tres->[0];
                $w += $tres->[1];
                $append = 0;
            }
        }
        push @res, $ansi if defined($ansi);
    }

    if ($return_width) {
        return [join("", @res), $w];
    } else {
        return join("", @res);
    }
}

sub ta_trunc {
    _ta_trunc(0, @_);
}

sub ta_mbtrunc {
    _ta_trunc(1, @_);
}

sub _ta_highlight {
    my ($is_all, $text, $needle, $color) = @_;

    # break into chunks
    my (@chptext, @chcode, @chsavedc); # chunk plain texts, codes, saved codes
    my $sc = "";
    my $plaintext = "";
    my @ch = ta_split_codes_single($text);
    while (my ($pt, $c) = splice(@ch, 0, 2)) {
        push @chptext , $pt;
        push @chcode  , $c;
        push @chsavedc, $sc;
        $plaintext .= $pt;
        if (defined($c) && $c =~ /m\z/) {
            if ($c eq "\e[0m") {
                $sc = "";
            } else {
                $sc .= $c;
            }
        }
    }
    #use Data::Dump; print "\@chptext: "; dd \@chptext; print "\@chcode: "; dd \@chcode; print "\@chsavedc: "; dd \@chsavedc;

    # gather a list of needles to highlight, with their positions
    my (@needle, @npos);
    if (ref($needle) eq 'Regexp') {
        my @m = $plaintext =~ /$needle/g;
        return $text unless @m;
        my $pos = 0;
        while ($pos < length($plaintext)) {
            my @pt;
            for (@m) {
                my $p = index($plaintext, $_, $pos);
                push @pt, [$p, $_] if $p >= 0;
            }
            last unless @pt;
            my $pmin = $pt[0][0];
            my $t = $pt[0][1];
            for (@pt) {
                if ($pmin > $_->[0] ||
                        $pmin==$_->[0] && length($t) < length($_->[1])) {
                    $pmin = $_->[0];
                    $t = $_->[1];
                }
            }
            push @needle, $t;
            push @npos  , $pmin;
            last unless $is_all;
            $pos = $pmin + length($t);
        }
    } else {
        my $pos = 0;
        while (1) {
            #say "D:finding '$needle' in '$plaintext' from pos '$pos'";
            my $p = index($plaintext, $needle, $pos);
            last if $p < 0;
            push @needle, $needle;
            push @npos  , $p;
            last unless $is_all;
            $pos = $p + length($needle);
            last if $pos >= length($plaintext);
        }
        return $text unless @needle;
    }
    #use Data::Dump; print "\@needle: "; dd \@needle; print "\@npos: "; dd \@npos;

    my @res;
    my $found = 1;
    my $pos = 0;
    my $i = 0;
    my $curneed = shift @needle;
    my $npos    = shift @npos;
  CHUNK:
    while (1) {
        last if $i >= @chptext;
        my $pos2  = $pos+length($chptext[$i])-1;
        my $npos2 = $npos+length($curneed)-1;
        #say "D: chunk=[$chptext[$i]], npos=$npos, npos2=$npos2, pos=$pos, pos2=$pos2";
        if ($pos > $npos2 || $pos2 < $npos || !$found) {
            #say "D:inserting chunk: [$chptext[$i]]";
            # no need to highlight
            push @res, $chptext[$i];
            push @res, $chcode[$i] if defined $chcode[$i];
            goto L1;
        }

        # there is chunk text at the left of needle?
        if ($pos < $npos) {
            my $pre = substr($chptext[$i], 0, $npos-$pos);
            #say "D:inserting pre=[$pre]";
            push @res, $pre;
        }

        my $npart = substr($curneed,
                           max(0, $pos-$npos),
                           min($pos2, $npos2)-max($pos, $npos)+1);
        if (length($npart)) {
            #say "D:inserting npart=[$npart]";
            push @res, $color, $npart;
            push @res, "\e[0m";
            #use Data::Dump; dd [$chsaved[$i], $chcode[$i]];
            push @res, $chsavedc[$i];
        }

        # is there chunk text at the right of needle?
        if ($npos2 <= $pos2) {
            #say "D:We have run past current needle [$curneed]";
            my $post = substr($chptext[$i], $npos2-$pos+1);

            if (@needle) {
                $curneed = shift @needle;
                $npos    = shift @npos;
                #say "D:Finding the next needle ($curneed) at pos $npos";
                $pos     = $npos2+1;
                $chptext[$i] = $post;
                $found = 1;
                redo CHUNK;
            } else {
                # we're done finding needle
                $found = 0;
            }

            if (!$found) {
                #say "D:inserting post=[$post]";
                push @res, $post;
                push @res, $chcode[$i] if defined $chcode[$i];
            }
        }

      L1:
        $pos = $pos2+1;
        $i++;
    }

    join "", @res;
}

sub ta_highlight {
    _ta_highlight(0, @_);
}

sub ta_highlight_all {
    _ta_highlight(1, @_);
}

sub ta_add_color_resets {
    my (@text) = @_;

    my @res;
    my $i = 0;
    my $savedc = "";
    for my $text (@text) {
        $i++;
        my $newt = $i > 1 && !$savedc ? "\e[0m" : $savedc;

        # break into chunks
        my @ch = ta_split_codes_single($text);
        while (my ($t, $c) = splice(@ch, 0, 2)) {
            $newt .= $t;
            if (defined($c) && $c =~ /m\z/) {
                $newt .= $c;
                if ($c eq "\e[0m") {
                    $savedc = "";
                } else {
                    $savedc .= $c;
                }
            }
        }

        $newt .= "\e[0m" if $savedc && $i < @text;
        push @res, $newt;
    }

    @res;
}

1;
# ABSTRACT: Routines for text containing ANSI escape codes

=encoding utf8

=head1 SYNOPSIS

 use Text::ANSI::Util qw(
     ta_add_color_resets
     ta_detect ta_highlight ta_highlight_all ta_length ta_mbpad ta_mbswidth
     ta_mbswidth_height ta_mbwrap ta_pad ta_split_codes ta_split_codes_single
     ta_strip ta_wrap);

 # detect whether text has ANSI escape codes?
 say ta_detect("red");         # => false
 say ta_detect("\e[31mred"); # => true

 # calculate length of text (excluding the ANSI escape codes)
 say ta_length("red");         # => 3
 say ta_length("\e[31mred"); # => 3

 # calculate visual width of text if printed on terminal (can handle Unicode
 # wide characters and exclude the ANSI escape codes)
 say ta_mbswidth("\e[31mred"); # => 3
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

 # pad (left, right, center) text to a certain width, handles multiple lines
 say ta_pad("foo", 10);                          # => "foo       "
 say ta_pad("foo", 10, "left");                  # => "       foo"
 say ta_pad("foo\nbarbaz\n", 10, "center", "."); # => "...foo....\n..barbaz..\n"

 # ditto, but handle wide characters
 say ta_mbpad(...);

 # truncate text to a certain width while still passing ANSI escape codes
 use Term::ANSIColor;
 my $text = color("red")."red text".color("reset"); # => "\e[31mred text\e[0m"
 say ta_trunc($text, 5);           # => "\e[31mred t\e[0m"

 # ditto, but handle wide characters
 say ta_mbtrunc(...);

 # highlight the first occurence of some string within text
 say ta_highlight("some text", "ome", "\x[7m\x[31m");

 # ditto, but highlight all occurrences
 say ta_highlight_all(...);


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

=back


=head1 FUNCTIONS

=head2 ta_detect($text) => BOOL

Return true if C<$text> contains ANSI escape codes, false otherwise.

=head2 ta_length($text) => INT

Count the number of bytes in $text, while ignoring ANSI escape codes. Equivalent
to C<< length(ta_strip($text) >>. See also: ta_mbswidth().

=head2 ta_length_height($text) => [INT, INT]

Like ta_length(), but also gives height (number of lines). For example, C<<
ta_length_height("foobar\nb\n") >> gives [6, 3].

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
ta_mbswidth_height("西爪哇\nb\n") >> gives [6, 3].

=head2 ta_strip($text) => STR

Strip ANSI escape codes from C<$text>, returning the stripped text.

=head2 ta_extract_codes($text) => STR

This is the opposite of ta_strip(), return only the ANSI codes in C<$text>.

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

Like ta_split_codes() but each ANSI escape code is split separately, instead of
grouped together.

=head2 ta_wrap($text, $width, \%opts) => STR

Like L<Text::WideChar::Util>'s wrap() except handles ANSI escape codes. Perform
color reset at the end of each line and a color restart at the start of
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

=back

*Performance: ~1000/s on my Core i5-2400 3.1GHz desktop for a ~1KB of text.

=head2 ta_mbwrap($text, $width, \%opts) => STR

Like ta_wrap(), but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters.

*Performance: ~1000/s on my Core i5-2400 3.1GHz desktop for a ~1KB of text.

=head2 ta_add_color_resets(@text) => LIST

Make sure that a color reset command (add C<\e[0m]>) to the end of each element
and a color restart (add all the color codes from the previous element, from the
last color reset) to the start of the next element, and so on. Return the new
list.

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

=head2 ta_mbpad => STR

Like ta_pad() but it uses ta_mbswidth() instead of ta_length(), so it can handle
wide characters.

=head2 ta_trunc($text, $width) => STR

Truncate C<$text> to C<$width> columns while still including all the ANSI escape
codes. This ensures that truncated text still reset colors, etc.

Does *not* handle multiline text; you can split text by C</\r?\n/> yourself.

=head2 ta_mbtrunc($text, $width) => STR

Like ta_trunc() but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters.

=head2 ta_highlight($text, $needle, $color) => STR

Highlight the first occurence of C<$needle> in C<$text> with <$color>, taking
care not to mess up existing colors.

C<$needle> can be a string or a Regexp object.

Performance: ~ 20k/s on my Core i5-2400 3.1GHz desktop for a ~ 1KB of text and a
needle of length ~ 7.

Implementation note: to not mess up colors, we save up all color codes from the
last reset (C<\e[0m]>) before inserting the highlight color + highlight text.
Then we issue C<\e[0m> and the saved up color code to return back to the color
state before the highlight is inserted. This is the same technique as described
in ta_add_color_resets().

=head2 ta_highlight_all($text, $needle, $color) => STR

Like ta_highlight(), but highlight all occurences instead of only the first.

Performance: ~ 4k/s on my Core i5-2400 3.1GHz desktop for a ~ 1KB of text and a
needle of length ~ 7 and number of occurences ~ 13.


=head1 FAQ

=head2 How do I truncate string based on number of characters?

You can simply use ta_trunc() even on text containing wide characters.
ta_trunc() uses Perl's length() which works on a per-character basis.

=head2 How do I highlight a string case-insensitively?

You can currently use a regex for the C<$needle> and use the C<i> modifier.
Example:

 use Term::ANSIColor;
 ta_highlight($text, qr/\b(foo)\b/i, color("bold red"));


=head1 TODOS

=over

=item * ta_split

A generalized version of ta_split_lines().

=back


=head1 SEE ALSO

L<Term::ANSIColor>

=cut
