#!perl -T

use 5.010001;
use strict;
use warnings;
use utf8;

use Test::More 0.98;
use Text::ANSI::Util qw(ta_detect ta_length ta_mbswidth ta_mbswidth_height ta_strip ta_mbwrap);

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
    is_deeply(ta_mbswidth_height("\x1b[31;47m我爱你\x1b[0m\nhello\n"), [6, 3]);
};

subtest "ta_mbswidth" => sub {
    is_deeply(ta_mbswidth(""), 0);
    is_deeply(ta_mbswidth("\x1b[0m"), 0);
    is_deeply(ta_mbswidth(" "), 1);
    is_deeply(ta_mbswidth(" \n"), 1);
    is_deeply(ta_mbswidth("\x1b[31;47m我爱你\x1b[0m\nhello\n"), 6);
};

subtest "ta_mbwrap" => sub {
};

DONE_TESTING:
done_testing();
