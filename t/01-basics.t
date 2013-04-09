#!perl -T

use 5.010001;
use strict;
use warnings;
use utf8;
use constant NL => "\n";

use POSIX;
use Test::More 0.98;
use Text::ANSI::Util qw(
                           ta_detect ta_length ta_mbpad ta_mbswidth
                           ta_mbswidth_height ta_mbtrunc ta_mbwrap ta_pad
                           ta_split_codes ta_strip ta_trunc ta_wrap);

# check if chinese locale is supported, otherwise bail
unless (POSIX::setlocale(&POSIX::LC_ALL, "zh_CN.utf8")) {
    plan skip_all => "Chinese locale not supported on this system";
}

subtest "ta_detect" => sub {
    ok(!ta_detect("a"), 'neg 1');
    ok(!ta_detect("\x1b"), 'neg 2');
    ok( ta_detect("\x1b[0m"), 'pos 1');
    ok( ta_detect("\x1b[31;47mhello\x1b[0m"), 'pos 2');
};

subtest "ta_strip" => sub {
    is(ta_strip(""), "");
    is(ta_strip("hello"), "hello");
    is(ta_strip("\x1b[31;47mhello\x1b[0m"), "hello");
};

subtest "ta_split_codes" => sub {
    is_deeply([ta_split_codes("")], []);
    is_deeply([ta_split_codes("a")], ["a"]);
    is_deeply([ta_split_codes("a\e[31m")], ["a", "\e[31m"]);
    is_deeply([ta_split_codes("\e[31ma")], ["", "\e[31m", "a"]);
    is_deeply([ta_split_codes("\e[31ma\e[0m")], ["", "\e[31m", "a", "\e[0m"]);
    is_deeply([ta_split_codes("\e[31ma\e[0mb")], ["", "\e[31m", "a", "\e[0m", "b"]);
    is_deeply([ta_split_codes("\e[31m\e[0mb")], ["", "\e[31m\e[0m", "b"]);
};

subtest "ta_length" => sub {
    is(ta_length(""), 0);
    is(ta_length("hello"), 5);
    is(ta_length("\x1b[0m"), 0);
    is(ta_length("\x1b[31;47mhello\x1b[0m"), 5);
};

subtest "ta_mbswidth_height" => sub {
    is_deeply(ta_mbswidth_height(""), [0, 0]);
    is_deeply(ta_mbswidth_height("\x1b[0m"), [0, 0]);
    is_deeply(ta_mbswidth_height(" "), [1, 1]);
    is_deeply(ta_mbswidth_height(" \n"), [1, 2]);
    is_deeply(ta_mbswidth_height("\x1b[31;47m你好吗\x1b[0m\nhello\n"), [6, 3]);
};

subtest "ta_mbswidth" => sub {
    is_deeply(ta_mbswidth(""), 0);
    is_deeply(ta_mbswidth("\x1b[0m"), 0);
    is_deeply(ta_mbswidth(" "), 1);
    is_deeply(ta_mbswidth(" \n"), 1);
    is_deeply(ta_mbswidth("\x1b[31;47m你好吗\x1b[0m\nhello\n"), 6);
};

# single paragraph
my $txt1 = <<_;
\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you want to go? I'll keep you company. Mr Goh,
I'm fine. You don't have to keep me company.
_
#qq--------10--------20--------30--------40--------50
my $txt1w =
qq|\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you|.NL.
qq|want to go? I'll keep you company. Mr|.NL.
qq|Goh, I'm fine. You don't have to keep me|.NL.
qq|company.|.NL;

# multiple paragraph
my $txt1b = <<_;
\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you want to go? I'll keep you company. Mr Goh,
I'm fine. You don't have to keep me company.

\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you want to go? I'll keep you company. Mr Goh,
I'm fine. You don't have to keep me company.
_
#qq--------10--------20--------30--------40--------50
my $txt1bw =
qq|\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you|.NL.
qq|want to go? I'll keep you company. Mr|.NL.
qq|Goh, I'm fine. You don't have to keep me|.NL.
qq|company.|.NL.NL.
qq|\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you|.NL.
qq|want to go? I'll keep you company. Mr|.NL.
qq|Goh, I'm fine. You don't have to keep me|.NL.
qq|company.|.NL;

# no terminating newline
my $txt1c = "\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you want to go? I'll keep you company. Mr Goh,
I'm fine. You don't have to keep...";
#qq--------10--------20--------30--------40--------50
my $txt1cw =
qq|\x1b[31;47mI\x1b[0m dont wan't to go home. Where do you|.NL.
qq|want to go? I'll keep you company. Mr|.NL.
qq|Goh, I'm fine. You don't have to keep...|;

subtest "ta_wrap" => sub {
    is(ta_wrap($txt1 , 40), $txt1w );
    is(ta_wrap($txt1b, 40), $txt1bw);
    is(ta_wrap($txt1c, 40), $txt1cw);
};

my $txt2 = <<_;
\x1b[31;47mI\x1b[0m dont wan't to go home. 我不想回家. Where do you want to go? I'll keep you
company. 那你想去哪里？我陪你. Mr Goh, I'm fine. 吴先生. 我没事. You don't have
to keep me company. 你不用陪我.
_
#qq--------10--------20--------30--------40--------50
my $txt2w =
qq|\x1b[31;47mI\x1b[0m dont wan't to go home. 我不想回家.|.NL.
qq|Where do you want to go? I'll keep you|.NL.
qq|company. 那你想去哪里？我陪你. Mr Goh,|.NL.
qq|I'm fine. 吴先生. 我没事. You don't have|.NL.
qq|to keep me company. 你不用陪我.|.NL;
subtest "ta_mbwrap" => sub {
    is(ta_mbwrap($txt2, 40), $txt2w);
};

subtest "ta_trunc" => sub {
    my $t = "\x1b[31m1\x1b[32m2\x1b[33m3\x1b[0m4";
    is(ta_trunc($t, 5), $t);
    is(ta_trunc($t, 4), $t);
    is(ta_trunc($t, 3), "\x1b[31m1\x1b[32m2\x1b[33m3\x1b[0m");
    is(ta_trunc($t, 2), "\x1b[31m1\x1b[32m2\x1b[33m\x1b[0m");
    is(ta_trunc($t, 1), "\x1b[31m1\x1b[32m\x1b[33m\x1b[0m");
    is(ta_trunc($t, 0), "\x1b[31m\x1b[32m\x1b[33m\x1b[0m");
};

subtest "ta_mbtrunc" => sub {
    my $t = "\x1b[31m不\x1b[32m用\x1b[33m陪\x1b[0m我";
    is(ta_mbtrunc($t, 9), $t);
    is(ta_mbtrunc($t, 8), $t);
    is(ta_mbtrunc($t, 7), "\x1b[31m不\x1b[32m用\x1b[33m陪\x1b[0m我"); # well, ...
    is(ta_mbtrunc($t, 6), "\x1b[31m不\x1b[32m用\x1b[33m陪\x1b[0m");
    is(ta_mbtrunc($t, 5), "\x1b[31m不\x1b[32m用\x1b[33m陪\x1b[0m"); # well, ...
    is(ta_mbtrunc($t, 4), "\x1b[31m不\x1b[32m用\x1b[33m\x1b[0m");
    is(ta_mbtrunc($t, 3), "\x1b[31m不\x1b[32m用\x1b[33m\x1b[0m"); # well, ...
    is(ta_mbtrunc($t, 2), "\x1b[31m不\x1b[32m\x1b[33m\x1b[0m");
    is(ta_mbtrunc($t, 1), "\x1b[31m不\x1b[32m\x1b[33m\x1b[0m"); # well, ...
    is(ta_mbtrunc($t, 0), "\x1b[31m\x1b[32m\x1b[33m\x1b[0m");
};

subtest "ta_pad" => sub {
    my $foo = "\x1b[31;47mfoo\x1b[0m";
    is(ta_pad(""    , 10), "          ", "empty");
    is(ta_pad("$foo", 10), "$foo       ");
    is(ta_pad("$foo", 10, "l"), "       $foo");
    is(ta_pad("$foo", 10, "c"), "   $foo    ");
    is(ta_pad("$foo", 10, "r", "x"), "${foo}xxxxxxx");
    is(ta_pad("${foo}12345678", 10), "${foo}12345678");
    is(ta_pad("${foo}12345678", 10, undef, undef, 1), "${foo}1234567");
};

subtest "ta_mbpad" => sub {
    my $foo = "\x1b[31;47m你好吗\x1b[0m";
    is(ta_mbpad(""    , 10), "          ", "empty");
    is(ta_mbpad("$foo", 10), "$foo    ");
    is(ta_mbpad("$foo", 10, "l"), "    $foo");
    is(ta_mbpad("$foo", 10, "c"), "  $foo  ");
    is(ta_mbpad("$foo", 10, "r", "x"), "${foo}xxxx");
    is(ta_mbpad("${foo}12345678", 10), "${foo}12345678");
    is(ta_mbpad("${foo}12345678", 10, undef, undef, 1), "${foo}1234");
};

DONE_TESTING:
done_testing();
