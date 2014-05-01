#!/usr/bin/env perl

BEGIN {
    unshift @INC,
    qq($ENV{PERLBREW_ROOT}/perls/perl-5.16.1/lib/site_perl/5.16.1/x86_64-linux),
    qq($ENV{PERLBREW_ROOT}/perls/perl-5.16.1/lib/site_perl/5.16.1),
    qq($ENV{PERLBREW_ROOT}/perls/perl-5.16.1/lib/5.16.1/x86_64-linux),
    qq($ENV{PERLBREW_ROOT}/perls/perl-5.16.1/lib/5.16.1),
}

use strict;
use warnings;

require(qq($ENV{HOME}/.elinks/hooks.pl));
my $filename = &url2filename($ARGV[0]);

print($filename);
