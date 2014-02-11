package Text::ANSI::Util;

use 5.010001;
use locale;
use strict;
use utf8;
use warnings;

use List::Util qw(min max);
use Text::WideChar::Util 0.10 qw(mbswidth mbtrunc);

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
    $res .= $1 while $text =~ /((?:$re)+)/go;
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
    # resets/replays), then grouping into words and paras, then doing wrapping.

    my @termst; # store term type, 's' (spaces), 'w' (word), 'c' (cjk word) or
                # 'p' (parabreak)
    my @terms;  # store the text (w/ codes); for ws, only store the codes
    my @pterms; # store the plaintext ver, but only for ws to check parabreak
    my @termsw; # store width of each term, only for non-ws
    my @termsc; # store color replay code
    {
        my @ch = ta_split_codes_single($text);
        my $crcode = ""; # code for color replay to be put at the start of line
        my $term      = '';
        my $pterm     = '';
        my $prev_type = '';
        while (my ($pt, $c) = splice(@ch, 0, 2)) {
            #use Data::Dump; print "D:chunk: "; dd [$pt, $c];

            # split into (CJK and non-CJK) words and spaces.

            my @s; # (WORD1, TYPE, ...) where type is 's' for space, 'c' for
                   # CJK word, or 'w' for non-CJK word
            while ($pt =~ /($Text::WideChar::Util::re_cjk+)|(\S+)|(\s+)/gox) {
                if ($1) {
                    push @s, $1, 'c';
                } elsif ($3) {
                    push @s, $3, 's';
                } else {
                    my $pt2 = $2;
                    while ($pt2 =~ /($Text::WideChar::Util::re_cjk_class+)|
                                    ($Text::WideChar::Util::re_cjk_negclass+)/gox) {
                        if ($1) {
                            push @s, $1, 'c';
                        } else {
                            push @s, $2, 'w';
                        }
                    }
                }
            }

            #use Data::Dump; say "D:s=",Data::Dump::dump(\@s);

            my $only_code = 1 if !@s;
            while (1) {
                my ($s, $s_type) = splice @s, 0, 2;
                $s_type //= '';
                last unless $only_code || defined($s);
                # empty text, only code
                if ($only_code) {
                    $s = "";
                    $term .= $c if defined $c;
                }
                #say "D:s=[$s]  prev_type=$prev_type \@ch=",~~@ch,"  \@s=",~~@s;

                if ($s_type && $s_type ne 's') {
                    if ($prev_type eq 's') {
                        #say "D:found word, completed previous ws [$term]";
                        push @termst, 's';
                        push @terms , $term;
                        push @pterms, $pterm;
                        push @termsw, undef;
                        push @termsc, $crcode;
                        # start new word
                        $pterm = ''; $term = '';
                    } elsif ($prev_type && $prev_type ne $s_type) {
                        #say "D:found a ".($s_type eq 'c' ? 'CJK':'non-CJK')." word, completed previous ".($prev_type eq 'c' ? 'CJK':'non-CJK')." word [$term]";
                        push @termst, $prev_type;
                        push @terms , $term;
                        push @pterms, $pterm;
                        push @termsw, $is_mb ? mbswidth($pterm):length($pterm);
                        push @termsc, $crcode;
                        # start new word
                        $pterm = ''; $term = '';
                    }
                    $pterm .= $s;
                    $term  .= $s; $term .= $c if defined($c) && !@s;
                    if (!@s && !@ch) {
                        #say "D:complete word because this is the last token";
                        push @termst, $s_type;
                        push @terms , $term;
                        push @pterms, "";
                        push @termsw, $is_mb ? mbswidth($pterm):length($pterm);
                        push @termsc, $crcode;
                    }
                } elsif (length($s)) {
                    if ($prev_type ne 's') {
                        #say "D:found ws, completed previous word [$term]";
                        push @termst, $prev_type;
                        push @terms , $term;
                        push @pterms, "";
                        push @termsw, $is_mb ? mbswidth($pterm):length($pterm);
                        push @termsc, $crcode;
                        # start new ws
                        $pterm = ''; $term = '';
                    }
                    $pterm .= $s;
                    $term  .= $c if defined($c) && !@s;
                    if (!@s && !@ch) {
                        #say "D:complete ws because this is the last token";
                        push @termst, 's';
                        push @terms , $term;
                        push @pterms, $pterm;
                        push @termsw, undef;
                        push @termsc, $crcode;
                    }
                }
                $prev_type = $s_type;

                if (!@s) {
                    if (defined($c) && $c =~ /m\z/) {
                        if ($c eq "\e[0m") {
                            #say "D:found color reset, emptying crcode";
                            $crcode = "";
                        } elsif ($c =~ /m\z/) {
                            #say "D:adding to crcode";
                            $crcode .= $c;
                        }
                    }
                    last if $only_code;
                }

            } # splice @s
        } # splice @ch
    }

    # mark parabreaks
    {
        my $i = 0;
        while ($i < @pterms) {
            if ($termst[$i] eq 's') {
                if ($pterms[$i] =~ /[ \t]*(\n(?:[ \t]*\n)+)([ \t]*)/) {
                    #say "D:found parabreak";
                    $pterms[$i] = $1;
                    $termst[$i] = 'p';
                    if ($i < @pterms-1) {
                        # stick color code to the beginning of next para
                        $terms [$i+1] = $terms[$i] . $terms [$i+1];
                        $terms [$i] = "";
                    }
                    if (length $2) {
                        #say "D:found space after parabreak, splitting";
                        splice @termst, $i+1, 0, "s";
                        splice @terms , $i+1, 0, "";
                        splice @pterms, $i+1, 0, $2;
                        splice @termsw, $i+1, 0, undef;
                        splice @termsc, $i+1, 0, $termsc[$i];
                        $i += 2;
                        next;
                    }
                }
            }
            $i++;
        }
    }

    #use Data::Dump::Color; my @d; for (0..$#terms) { push @d, {type=>$termst[$_], term=>$terms[$_], pterm=>$pterms[$_], termc=>$termsc[$_], termw=>$termsw[$_], } } dd \@d;
    #return;

    #use Data::Dump; say "D:termst=".Data::Dump::dump(\@termst);
    #use Data::Dump; say "D:terms =".Data::Dump::dump(\@terms);
    #use Data::Dump; say "D:pterms=".Data::Dump::dump(\@pterms);
    #use Data::Dump; say "D:termsw=".Data::Dump::dump(\@termsw);
    #use Data::Dump; say "D:termsc=".Data::Dump::dump(\@termsc);

    my ($maxww, $minww);

    # now we perform wrapping

    my @res;
    {
        my $tw = $opts->{tab_width} // 8;
        die "Please specify a positive tab width" unless $tw > 0;
        my $optfli  = $opts->{flindent};
        my $optfliw = Text::WideChar::Util::_get_indent_width($is_mb, $optfli, $tw) if defined $optfli;
        my $optsli  = $opts->{slindent};
        my $optsliw = Text::WideChar::Util::_get_indent_width($is_mb, $optsli, $tw) if defined $optsli;
        my $pad = $opts->{pad};
        my $x = 0;
        my $y = 0;
        my ($fli, $sli, $fliw, $sliw);
        my $is_parastart = 1;
        my $line_has_word = 0;
        my ($termt, $prev_t);
      TERM:
        for my $i (0..$#terms) {
            $prev_t = $termt if $i;
            $termt = $termst[$i];
            my $term  = $terms[$i];
            my $pterm = $pterms[$i];
            my $termw = $termsw[$i];
            my $crcode = $i > 0 ? $termsc[$i-1] : "";
            #say "D:term=[", ($termt eq 'w' ? $term : $pterm), "] ($termt)";

            # end of paragraph
            if ($termt eq 'p') {
                my $numnl = 0;
                $numnl++ while $pterm =~ /\n/g;
                for (1..$numnl) {
                    push @res, "\e[0m" if $crcode && $_ == 1;
                    push @res, " " x ($width-$x) if $pad;
                    push @res, "\n";
                    $x = 0;
                    $y++;
                }
                $line_has_word = 0;
                $x = 0;
                $is_parastart = 1;
                next TERM;
            }

            if ($is_parastart) {
                # this is the start of paragraph, determine indents
                if (defined $optfli) {
                    $fli  = $optfli;
                    $fliw = $optfliw;
                } else {
                    if ($termt eq 's') {
                        $fli  = $pterm;
                        $fliw = Text::WideChar::Util::_get_indent_width($is_mb, $fli, $tw);
                    } else {
                        $fli  = "";
                        $fliw = 0;
                    }
                    #say "D:deduced fli from text [$fli] ($fliw)";
                    my $j = $i;
                    $sli = undef;
                    while ($j < @terms && $termst[$j] ne 'p') {
                        if ($termst[$j] eq 's') {
                            if ($pterms[$j] =~ /\n([ \t]+)/) {
                                $sli  = $1;
                                $sliw = Text::WideChar::Util::_get_indent_width($is_mb, $sli, $tw);
                                last;
                            }
                        }
                        $j++;
                    }
                    if (!defined($sli)) {
                        $sli  = "";
                        $sliw = 0;
                    }
                    #say "D:deduced sli from text [$sli] ($sliw)";
                    die "Subsequent indent must be less than width" if $sliw >= $width;
                }

                #say "D:inserting the fli [$fli] ($fliw)";
                push @res, $fli;
                $x += $fliw;
            } # parastart

            $is_parastart = 0;

            if ($termt eq 's') {
                # just print the codes
                push @res, $term;

                # maintain terminating newline
                if ($pterm =~ /\n/ && $i == $#terms) {
                    push @res, "\e[0m" if $crcode;
                    push @res, " " x ($width-$x) if $pad;
                    push @res, "\n";
                    $line_has_word = 0;
                }
            }

            if ($termt ne 's') {
                # we need to chop long words
                my @words;
                my @wordsw;
                my @wordst; # c if cjk, w if not
                my @wordswsb; # whether there are ws before the word
                my $j = 0;
                my $c = ""; # see below for explanation
                while (1) {
                    $j++;
                    # most words shouldn't be that long. and we don't need to
                    # truncate long CJK word first here because it will get
                    # truncated later.
                    if ($termw <= $width-$sliw || $termt eq 'c') {
                        push @words   , $c . $term;
                        push @wordsw  , $termw;
                        push @wordst  , $termt;
                        push @wordswsb, ($prev_t && $prev_t eq 's')?1:0;
                        last;
                    }
                    #use Data::Dump; print "D:truncating long word "; dd $term;
                    my $res = $is_mb ? ta_mbtrunc($term, $width-$sliw, 1) :
                        ta_trunc($term, $width-$sliw, 1);

                    my ($tword, $twordw);
                    if ($j == 1) {
                        $tword  = $res->[0];
                        $twordw = $res->[1];
                    } else {
                        # since ta_{,mb}trunc() adds the codes until the end of
                        # the word, to avoid messing colors, for the second word
                        # and so on we need to replay colors by prefixing with:
                        # \e[0m (reset) + $crcode + (all the codes from the
                        # start of the long word up until the truncated
                        # position, stored in $c).
                        #
                        # there might be faster way, but it is expected that
                        # long words are not that common.
                        $tword  = ($crcode ? "\e[0m" . $crcode : "") .
                            $c . $res->[0];
                        $twordw = $res->[1];
                    }
                    $c .= ta_extract_codes(substr($term, 0, $res->[2]));
                    #use Data::Dump; print "D:truncated word is "; dd $tword;

                    push @words   , $tword;
                    push @wordsw  , $twordw;
                    push @wordst  , $termt;
                    push @wordswsb, $j == 1 ? (($prev_t && $prev_t eq 's')?1:0) : 0;
                    $term  = substr($term, $res->[2]);
                    $termw = $is_mb ? _ta_mbswidth0($term) : ta_length($term);
                }

                #use Data::Dump; print "D:words="; dd \@words; print "D:wordsw="; dd \@wordsw; print "D:wordswsb="; dd \@wordswsb;

                # the core of the wrapping algo
                for my $word (@words) {
                    my $wordw = shift @wordsw;
                    my $wordt = shift @wordst;
                    my $ws_before = shift @wordswsb;
                    #say "D:x=$x word=$word wordw=$wordw wordt=$wordt ws_before=$ws_before line_has_word=$line_has_word width=$width";

                    $maxww = $wordw if !defined($maxww) || $maxww < $wordw;
                    $minww = $wordw if !defined($minww) || $minww > $wordw;

                    if ($x + ($line_has_word ? 1:0) + $wordw <= $width) {
                        if ($line_has_word && $ws_before) {
                            push @res, " ";
                            $x++;
                        }
                        push @res, $word;
                        $x += $wordw;
                    } else {
                        # line break
                        if ($wordt eq 'c') {
                            # a CJK word can be line-broken
                            my $res = ta_mbtrunc($word, $width-$x, 1);
                            push @res, $res->[0];
                            #say "D:truncated CJK word: $word (".length($word)."), ".($width-$x)." -> $res->[0] (".length($res->[0]).") & $res->[1], remaining=$res->[3] (".length($res->[3]).")";
                            $word = $res->[3];
                            $wordw = _ta_mbswidth0($res->[3]);
                        } else {
                            push @res, "\e[0m" if $crcode;
                        }
                        push @res, " " x ($width-$x) if $pad;
                        push @res, "\n";
                        push @res, $crcode;
                        push @res, $sli, $word;
                        $x = $sliw + $wordw;
                        $y++;
                    }
                    $line_has_word++;
                }

            }
        } # for term
        push @res, " " x ($width-$x) if $line_has_word && $pad;
    }

    if ($opts->{return_stats}) {
        return [join("", @res), {
            max_word_width => $maxww,
            min_word_width => $minww,
        }];
    } else {
        return join("", @res);
    }
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
    my ($is_mb, $text, $width, $return_extra) = @_;

    # return_extra (undocumented): if set to 1, will return [truncated_text,
    # visual width, length(chars) up to truncation point, rest of the text not
    # included]

    my $w = $is_mb ? _ta_mbswidth0($text) : ta_length($text);
    if ($w <= $width) {
        return $return_extra ? [$text, $w, length($text), ''] : $text;
    }
    my @p = ta_split_codes($text);
    my @res;
    my $append = 1; # whether we should add more text
    my $code4rest = '';
    my $rest = '';
    $w = 0;
    my $c = 0;
    #use Data::Dump; dd \@p;
    while (my ($t, $ansi) = splice @p, 0, 2) {
        #say "D: t=<$t>, \@p=", ~~@p, ", code4rest=<$code4rest>, rest=<$rest>";
        if ($append) {
            my $tw = $is_mb ? mbswidth($t) : length($t);
            #say "D: tw=$tw";
            if ($w+$tw <= $width) {
                push @res, $t;
                $w += $tw;
                $c += length($t);
                $append = 0 if $w == $width;
                #say "D:end1" unless $append;
            } else {
                my $tres = $is_mb ?
                    mbtrunc($t, $width-$w, 1) :
                        [substr($t, 0, $width-$w), $width-$w, $width-$w];
                #use Data::Dump; dd $tres;
                push @res, $tres->[0];
                $w += $tres->[1];
                $c += $tres->[2];
                $rest = substr($t, $tres->[2]);
                $append = 0;
                #say "D:end2";
            }
        } else {
            $rest .= $t;
        }
        if (defined $ansi) {
            push @res, $ansi;
            $c += length($ansi) if $append;
            $code4rest .= $ansi if $append;
            $rest .= $ansi unless $append;
        }
    }

    if ($return_extra) {
        return [join("", @res), $w, $c, $code4rest . $rest];
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
            } elsif ($c =~ /m\z/) {
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
                } elsif ($c =~ /m\z/) {
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

=head1 SYNOPSIS

 use Text::ANSI::Util qw(
     ta_add_color_resets
     ta_detect ta_highlight ta_highlight_all ta_length ta_mbpad ta_mbswidth
     ta_mbswidth_height ta_mbwrap ta_pad ta_split_codes ta_split_codes_single
     ta_strip ta_wrap);

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

=head2 ta_detect($text) => BOOL

Return true if C<$text> contains ANSI escape codes, false otherwise.

=head2 ta_length($text) => INT

Count the number of characters in $text, while ignoring ANSI escape codes.
Equivalent to C<< length(ta_strip($text) >>. See also: ta_mbswidth().

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

=head2 ta_mbwrap($text, $width, \%opts) => STR

Like ta_wrap(), but it uses ta_mbswidth() instead of ta_length(), so it can
handle wide characters.

Performance: ~300/s on my Core i5 1.7GHz laptop for a ~1KB of text (with zero to
moderate amount of color codes). As a comparison, Text::WideChar::Util's
mbwrap() can do about 650/s.

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

=head2 ta_mbpad($text, $width[, $which[, $padchar[, $truncate]]]) => STR

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

Implementation note: to not mess up colors, we save up all color codes from the
last reset (C<\e[0m>) before inserting the highlight color + highlight text.
Then we issue C<\e[0m> and the saved up color code to return back to the color
state before the highlight is inserted. This is the same technique as described
in ta_add_color_resets().

=head2 ta_highlight_all($text, $needle, $color) => STR

Like ta_highlight(), but highlight all occurences instead of only the first.


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

=item * ta_split($re, $text)

=item * ta_match($re, $text)

Regex search.

=item * ta_replace($re, $str, $text) (and ta_replace_all)

Regex substitution.

=back


=head1 SEE ALSO

L<Term::ANSIColor>

L<Text::ANSITable> uses this module. In fact, this module was first created
specifically for Text::ANSITable.

http://en.wikipedia.org/wiki/ANSI_escape_code

=cut
