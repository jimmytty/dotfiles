#!/usr/bin/perl

use strict;
use warnings;
use diagnostics;

use Test::More q(no_plan);

require_ok(q(hooks.pl));

#==============================================================================
# url2filename subroutine test
#==============================================================================
is( &url2filename(
        q(http://www.test.com/subdir/index.pl?var1=x&var2=y&var3=%2E)),
    qq($ENV{ELINKS_CACHE}/http_www.test.com_subdir_index.pl?var1=x&var2=y&var3=%2E.html)
);
my $expect = qq($ENV{ELINKS_CACHE}/http_www.google.com.html);
is( &url2filename(q(http://www.google.com/)), $expect );
is( &url2filename(q(http://www.google.com)),  $expect );
is( &url2filename(q(file:///home/local/file.html)),
    q(/home/local/file.html) );
is( &url2filename(
        q(http://www.test.com/subdir/llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll)
    ),
    qq($ENV{ELINKS_CACHE}/http_www.test.com_subdir_lllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll.html)
);

#==============================================================================
# cache_file subroutine test
#==============================================================================
my $url  = q(gopher://teste.html);
my $file = qq($ENV{ELINKS_CACHE}/gopher_teste.html.html);
unlink($file) if ( -f $file );
my $html = q(<html><head></head><body></body></html>);
&cache_file( $url, $html );
open( my $fh, q(<), $file ) or die(qq(cannot open file $file: $!\n));
my $body = <$fh>;
close($fh);
is( $body, $html );
