package Text::ANSI::WideUtil;

# DATE
# VERSION

use 5.010001;
use strict 'subs', 'vars';
use warnings;

use Text::WideChar::Util qw(mbswidth mbtrunc);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(
                       ta_mbpad
                       ta_mbsubstr
                       ta_mbswidth
                       ta_mbswidth_height
                       ta_mbtrunc
                       ta_mbwrap
               );

use Text::ANSI::BaseUtil ();

our $re = $Text::ANSI::BaseUtil::re;
*{$_} = \&{"Text::ANSI::BaseUtil::$_"} for @EXPORT_OK;

1;
# ABSTRACT: Routines for text containing ANSI color codes (wide functions only)

=head1 FUNCTIONS

# INSERT_BLOCK: lib/Text/ANSI/Util.pm pod_wide_functions
