package Text::ANSI::NonWideUtil;

# DATE
# VERSION

use 5.010001;
use strict 'subs', 'vars';
use warnings;

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
# ABSTRACT: Routines for text containing ANSI escape codes (non-wide functions only)

=head1 FUNCTIONS

# INSERT_BLOCK: lib/Text/ANSI/Util.pm pod_nonwide_functions
