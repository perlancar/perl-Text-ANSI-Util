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
# ABSTRACT: Routines for text containing ANSI color codes (non-wide functions only)

=head1 DESCRIPTION

This module provides routines for dealing with text that contains ANSI color
codes, e.g. for determining its length/width (excluding the color codes),
stripping the color codes, extracting the color codes, and so on.

There is also a wide variant: L<Text::ANSI::WideUtil>. The difference is that
::WideUtil can handle wide (full-width) Unicode characters, while ::NonWideUtil
can also handle normal/halfwidth/ASCII characters.


=head1 FUNCTIONS

# INSERT_BLOCK: lib/Text/ANSI/Util.pm pod_nonwide_functions


=head1 SEE ALSO

L<Text::ANSI::WideUtil>
