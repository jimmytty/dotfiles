#!/usr/bin/env perl

BEGIN {
    my $cmd = q{perl -E'say for grep {$_ ne q(.)} @INC'};
    unshift @INC, split /\s+/s, qx{$cmd};
}

use common::sense;
use autodie;
use Encode;
use URI;
use URI::QueryParam;
use URI::Escape;
use HTML::TreeBuilder::LibXML;
#use HTML::Entities;
use Text::Unidecode;
use Unicode::CheckUTF8 qw(is_utf8);
use Data::Dumper;

use Log::Log4perl qw(:easy);
my $logfile = dirname($0) . q(/hooks_log4perl.conf);
Log::Log4perl::init_and_watch( $logfile, 10 );
my $logger = Log::Log4perl->get_logger(q(hooks));

#==============================================================================
# LOCAL FUNCTIONS
#==============================================================================

sub url2filename {    # kill me!
    my $url = shift();

    my $uri    = URI->new($url);
    my $scheme = $uri->scheme();
    my $path   = $uri->path();

    return ( undef() ) unless ($scheme);
    return ($path) if ( $scheme eq q(file) );

    my $opaque = $uri->opaque();
    use File::Basename;
    use File::Spec::Unix;
    my @path = File::Spec->splitdir(qq($opaque));
    @path = grep( /./, @path );
    my $filename = join( q(_), $scheme, @path );
    $filename = substr( $filename, 0, 250 ) if ( length($filename) > 250 );
    $filename .= q(.html);
    my $cachefile = qq($ENV{ELINKS_CACHE}/$filename);

    return ($cachefile);

} ## end sub url2filename

sub cache_file {
    my $url  = shift();
    my $html = shift();

    my $cachefile = &url2filename($url);

    if ($cachefile) {
        if (substr( $cachefile, 0, length( $ENV{ELINKS_CACHE} ) ) eq
            $ENV{ELINKS_CACHE} )
        {
            open( my $cachefh, q(>), $cachefile )
                or $logger->error(
                qq(ERROR: cannot open file "$cachefile": $!\n));
            print( $cachefh $html );
            close($cachefh);
        }
    }

} ## end sub cache_file

#==============================================================================
# HOOKS
#==============================================================================

sub pre_format_html_hook {
    my ( $url, $html ) = splice @_;

    &cache_file( $url, $html );

    my $uri  = URI->new($url);
    my $tree = HTML::TreeBuilder::LibXML->new();
    $tree->store_comments();
    $tree->parse($html);

    if (   $uri->host() eq q(github.com)
        && $uri->path(q(/WUB/issues)) )
    {
        my @xpath = (
            q{//script},                                #
            q{//div[@id="keyboard_shortcuts_pane"]},    #
            q{//div[@id="markdown-help"]},              #
            q{//ul[h4[normalize-space(string(.))='Edit Labels']]},
            q{//div[@class="edit-color-label-form js-new-label-form"]},      #
            q{//div[@id="footer"]},                                          #
            q{//div[@id="wrapper"]/div/div[@class="container clearfix"]},    #
            q{//div[@class="hentry"]/div[@class="pagehead repohead instapaper_ignore readability-menu"]},
        );

        while ( my $trash
            = $tree->findnodes( q[(] . join( q(|), @xpath ) . q[)] )->[0] )
        {
            $trash->delete();
        }
    } ## end if ( $uri->host() eq q(github.com)...)

    $html = $tree->as_HTML();
    $tree->eof();
    $tree = $tree->delete();

    return $html;
} ## end sub pre_format_html_hook

sub goto_url_hook {
    my $url  = $_[0];
    my $file = q();
    my $f    = sub { ($file) = grep( -e, glob( $_[0] ) ) };

    foreach ($url) {
        when (q(aria2c))     { $f->(q(/usr/doc/aria2-*/README.html)) }
        when (q(asciidoc))   { $f->(q(/usr/doc/asciidoc-*/doc/)) }
        when (q(bind))       { $f->(q(/usr/doc/bind-*/arm/Bv*ARM.html)) }
        when (q(bzip2))      { $f->(q(/usr/doc/bzip2-*/manual.html)) }
        when (q(clisp))      { $f->(q(/usr/doc/clisp-*/doc/)) }
        when (q(cron))       { $f->(q(/usr/doc/dcron-*/)) }
        when (q(ctags))      { $f->(q(/usr/doc/ctags-*/)) }
        when (q(cups))       { $f->(q(/usr/doc/cups-*/index.html)) }
        when (q(cyrus-sasl)) { $f->(q(/usr/doc/cyrus-sasl-*/doc/index.html)) }
        when (q(docbook-utils)) {
            $f->(q(/usr/doc/docbook-utils-*/html/index.html))
        }
        when (q(elinks)) { $f->(q(/usr/doc/elinks-*/doc/)) }
        when (q(elinks.manual)) {
            $f->(q(/usr/doc/elinks-*/doc/html/manual.html-chunked/index.html))
        }
        when (q(enscript))    { $f->(q(/usr/doc/enscript-*/FAQ.html)) }
        when (q(espeak))      { $f->(q(/usr/doc/espeak-*/index.html)) }
        when (q(expat))       { $f->(q(/usr/doc/expat-*/reference.html)) }
        when (q(expect))      { $f->(q(/usr/doc/expect-*/)) }
        when (q(imagemagick)) { $f->(q(/usr/doc/ImageMagick-*/index.html)) }
        when (q(kbd))         { $f->(q(/usr/doc/kbd-*/kbd.FAQ.html)) }
        when (q(linux-faqs)) {
            $f->(q(/usr/doc/Linux-FAQs/Threads-FAQ/html/index.html))
        }
        when (q(linux-howtos)) { $f->(q(/usr/doc/Linux-HOWTOs/INDEX.html)) }
        when (q(m4))           { $f->(q(/usr/doc/m4-*/)) }
        when (q(mpg123))       { $f->(q(/usr/doc/mpg123-*/)) }
        when (q(mplayer)) { $f->(q(/usr/doc/MPlayer-*/HTML/en/MPlayer.html)) }
        when (q(mutt))    { $f->(q(/usr/doc/mutt/)) }
        when (q(mutt.manual))   { $f->(q(/usr/doc/mutt/manual.txt)) }
        when (q(muttng))        { $f->(q(/usr/doc/muttng-*/)) }
        when (q(muttng.manual)) { $f->(q(/usr/doc/muttng-*/html/index.html)) }
        when (q(mysql))         { $f->(q(/usr/doc/mysql-*/)) }
        when ( [qw[ntp ntpd ntpdate ntpc]] ) {
            $f->(q(/usr/doc/ntp-*/html/index.html))
        }
        when (q(pcre))     { $f->(q(/usr/doc/pcre-*/html/index.html)) }
        when (q(postgis))  { $f->(q(/usr/doc/postgis-*/html/postgis.html)) }
        when (q(rsync))    { $f->(q(/usr/doc/rsync-*/)) }
        when (q(sendmail)) { $f->(q(/usr/doc/sendmail-*/)) }
        when (q(sgml-common)) {
            $f->(q(/usr/doc/sgml-common-*/html/index.html))
        }
        when (q(sgmlspl)) { $f->(q(/usr/doc/sgmlspl-*/sgmlspl/sgmlspl.html)) }
        when (q(sysstat)) { $f->(q(/usr/doc/sysstat-*/)) }
        when (q(tidy))    { $f->(q(/usr/doc/tidy-*/htmldoc/)) }
        when (q(transfig)) { $f->(q(/usr/doc/transfig-*/manual/)) }
        when ( [qw[netcat nc]] )   { $f->(q(/usr/doc/nc-*/)) }
        when ( [qw[openssl ssl]] ) { $f->(q(/usr/doc/openssl-*/)) }
        when ( [qw[p7zip 7zip]] )  { $f->(q(/usr/doc/p7zip-*/DOCS/)) }
        when (q(wpa_supplicant)) { $f->(q(/usr/doc/wpa_supplicant-*/)) }
        when ( [qw[postgresql pgsql psql]] ) {
            $f->(q(/usr/doc/postgresql-*/html/index.html))
        }
        when ( [qw[subversion svn]] ) { $f->(q(/usr/doc/subversion-*/)) }
        when (q(xz))  { $f->(q(/usr/doc/xz-*/)) }
        when (q(zsh)) { $f->(q(/usr/doc/zsh-*/html-docs/zsh.html)) }
        when (q(lua)) { $f->(q(/usr/doc/lua-*/html/readme.html)) }
        when (q(linuxdoc-tools)) {
            $f->(q(/usr/doc/linuxdoc-tools-*/html/guide.html))
        }
        when (q(lilo))    { $f->(q(/usr/doc/lilo-*/)) }
        when (q(libxslt)) { $f->(q(/usr/doc/libxslt-*/html/index.html)) }
        when (q(libxml2)) { $f->(q(/usr/doc/libxml2-*/)) }
        when (q(libxml2.manual)) {
            $f->(q(/usr/doc/libxml2-*/html/html/index.html))
        }
        when (q(libxml2.tutorial)) {
            $f->(q(/usr/doc/libxml2-*/html/tutorial/index.html))
        }
        when (q(libnet)) { $f->(q(/usr/doc/libnet-*/)) }
        when (q(iptraf)) { $f->(q(/usr/doc/iptraf-*/)) }
        when (q(iptraf.manual)) {
            $f->(q(/usr/doc/iptraf-*/Documentation/manual.html))
        }
        when (q(iproute2))     { $f->(q(/usr/doc/iproute2-*o/)) }
        when (q(hal))          { $f->(q(/usr/doc/hal-*/spec/hal-spec.html)) }
        when (q(sjeng))        { $f->(q(/usr/doc/Sjeng-Free-*/)) }
        when (q(autoconf))     { $f->(q(/usr/doc/autoconf-*/)) }
        when (q(bash))         { $f->(q(/usr/doc/bash-*/)) }
        when (q(bc))           { $f->(q(/usr/doc/bc-*/)) }
        when (q(bitlbee))      { $f->(q(/usr/doc/bitlbee-*/)) }
        when (q(bridge-utils)) { $f->(q(/usr/doc/bridge-utils-*/doc/)) }
        when (q(curl))         { $f->(q(/usr/doc/curl-*/)) }
        when (q(curl.manual))  { $f->(q(/usr/doc/curl-*/MANUAL)) }
        when (q(dc3dd))        { $f->(q(/usr/doc/dc3dd-*/)) }
        when (q(dhcp))         { $f->(q(/usr/doc/dhcp-*/)) }
        when (q(dnsmasqs))     { $f->(q(/usr/doc/dnsmasq-*/)) }
        when (q(docboolk-xsl)) { $f->(q(/usr/doc/docbook-xsl-*/index.html)) }
        when (q(doxygen))      { $f->(q(/usr/doc/doxygen-*/html/index.html)) }
        when (q(dvd+rw-tools)) { $f->(q(/usr/doc/dvd+rw-tools-*/index.html)) }
        when (q(ed))           { $f->(q(/usr/doc/ed-*/)) }
        when (q(elm))          { $f->(q(/usr/doc/elm-*/)) }
        when (q(fetchmail))    { $f->(q(/usr/doc/fetchmail-*/)) }
        when (q(flac))         { $f->(q(//usr/doc/flac-*/index.html)) }
        when (q(gc))           { $f->(q(/usr/doc/gc-*/)) }
        when ( [ q(about:config), q(config) ] ) {
            $f->(qq($ENV{HOME}/.elinks/elinks.conf))
        }
        when ( [ q(about:hooks), q(hooks) ] ) {
            $f->(qq($ENV{HOME}/.elinks/hooks.pl))
        }
        when ( [ q(about:cache), q(cache) ] ) {
            $f->(qq($ENV{ELINKS_CACHE}/))
        }
        when (q(git)) { $f->(q(/usr/doc/git-*/Documentation/)) }
        when ( [qw[slack slackware]] ) {
            $f->(qq($ENV{HOME}/doc/slackbook/html/index.html))
        }
        when (q(R))     { $f->(qq(/usr/doc/R-*/)) }
        when (q(tetex)) { $f->(qq(/usr/doc/tetex-*/index.html)) }
    } ## end foreach ($url)

    $url = $file if ($file);
    return ($url);
} ## end sub goto_url_hook

#sub follow_url_hook {}
#sub proxy_for_hook {}
#sub quit_hook {}

1;
