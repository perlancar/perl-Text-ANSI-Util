#!perl -T

use 5.010001;
use strict;
use warnings;
use utf8;

use Test::More 0.98;
use Text::ANSI::Util qw(ta_detect ta_length ta_mbswidth ta_mbswidth_height ta_mbwrap ta_strip ta_wrap);

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

my $txt1 = <<'_';
I dont wan't to go home. Where do you want to go? I'll keep you company. Mr Goh,
I'm fine. You don't have to keep me company.
_
# --------10--------20--------30--------40--------50
my $txt1w =
q[I dont wan't to go home. Where do you]."\n".
q[want to go? I'll keep you company. Mr]."\n".
q[Goh, I'm fine. You don't have to keep me]."\n".
q[company.];
subtest "ta_wrap" => sub {
    is(ta_wrap($txt1, 40), $txt1w);
};

my $txt2 = <<'_';
I dont wan't to go home. 我不想回家. Where do you want to go? I'll keep you
company. 那你想去哪里？我陪你. Mr Goh, I'm fine. 吴先生. 我没事. You don't have
to keep me company. 你不用陪我.
_
# --------10--------20--------30--------40--------50
my $txt2w =
q[I dont wan't to go home. 我不想回家.]."\n".
q[Where do you want to go? I'll keep you]."\n".
q[company. 那你想去哪里？我陪你. Mr Goh,]."\n".
q[I'm fine. 吴先生. 我没事. You don't have]."\n".
q[to keep me company. 你不用陪我.];
subtest "ta_mbwrap" => sub {
    is(ta_mbwrap($txt2, 40), $txt2w);
};

DONE_TESTING:
done_testing();
