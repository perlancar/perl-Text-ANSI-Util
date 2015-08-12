package Text::ANSI::BaseUtil;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use List::Util qw(min max);

# used to find/strip escape codes from string
our $re       = qr/\e\[.+?m/s;
our $re_mult  = qr/(?:\e\[.+?m)+/s;

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
        push @lens, $is_mb ? Text::WideChar::Util::mbswidth($e) : length($e);
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
    $res .= $1 while $text =~ /($re_mult)/go;
    $res;
}

sub ta_split_codes {
    my $text = shift;
    return split(/($re_mult)/o, $text);
}

sub ta_split_codes_single {
    my $text = shift;
    return split(/($re)/o, $text);
}

# same like _ta_mbswidth, but without handling multiline text
sub _ta_mbswidth0 {
    my $text = shift;
    Text::WideChar::Util::mbswidth(ta_strip($text));
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
                        push @termsw, $is_mb ? Text::WideChar::Util::mbswidth($pterm):length($pterm);
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
                        push @termsw, $is_mb ? Text::WideChar::Util::mbswidth($pterm):length($pterm);
                        push @termsc, $crcode;
                    }
                } elsif (length($s)) {
                    if ($prev_type ne 's') {
                        #say "D:found ws, completed previous word [$term]";
                        push @termst, $prev_type;
                        push @terms , $term;
                        push @pterms, "";
                        push @termsw, $is_mb ? Text::WideChar::Util::mbswidth($pterm):length($pterm);
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
                        while (1) {
                            if ($wordt eq 'c') {
                                # a CJK word can be line-broken
                                my $res;
                                if ($ws_before) {
                                    $res = ta_mbtrunc($word, $width-$x-1, 1);
                                    push @res, " ", $res->[0];
                                } else {
                                    $res = ta_mbtrunc($word, $width-$x, 1);
                                    push @res, $res->[0];
                                }
                                #say "D:truncated CJK word: $word (".length($word)."), ".($width-$x)." -> $res->[0] (".length($res->[0]).") & $res->[1], remaining=$res->[3] (".length($res->[3]).")";
                                $word = $res->[3];
                                $wordw = _ta_mbswidth0($res->[3]);
                            } else {
                                push @res, "\e[0m" if $crcode;
                            }
                            push @res, " " x ($width-$x) if $pad;
                            push @res, "\n";
                            $y++;
                            push @res, $crcode;
                            push @res, $sli;

                            if ($sliw + $wordw <= $width) {
                                push @res, $word;
                                $x = $sliw + $wordw;
                                last;
                            } else {
                                # word still too long, break again
                                $x = $sliw;
                            }
                        }
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
            my $tw = $is_mb ? Text::WideChar::Util::mbswidth($t) : length($t);
            #say "D: tw=$tw";
            if ($w+$tw <= $width) {
                push @res, $t;
                $w += $tw;
                $c += length($t);
                $append = 0 if $w == $width;
                #say "D:end1" unless $append;
            } else {
                my $tres = $is_mb ?
                    Text::WideChar::Util::mbtrunc($t, $width-$w, 1) :
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

sub _ta_substr {
    my $is_mb = shift;
    my $str   = shift;
    my $pos   = shift;
    my $len   = shift;

    my $res1 = _ta_trunc($is_mb, $str, $pos, 1);
    my $res2 = _ta_trunc($is_mb, $res1->[3], $len, 1);

    if (@_) {
        # left + replacement + right
        return $res1->[0] . $_[0] . $res2->[3];
    } else {
        return $res2->[0];
    }
}

sub ta_substr {
    _ta_substr(0, @_);
}

sub ta_mbsubstr {
    _ta_substr(1, @_);
}


1;
# ABSTRACT: Base for Text::ANSI::{Util,NonWideUtil,WideUtil}

=for Pod::Coverage .+
