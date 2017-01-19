#!/usr/bin/env perl

BEGIN {
    my $cmd = q{perl -E'say for grep {$_ ne q(.)} @INC'};
    unshift @INC, split /\s+/s, qx{$cmd};
}

use common::sense;
use autodie;
use experimental q(smartmatch);
use Encode;
use File::Basename;
use File::Spec::Unix;
use List::Util qw[first];
use Time::Piece;
use HTML::Element;
use HTML::Entities;
use HTML::TreeBuilder::LibXML;
use Text::Unidecode;
use URI;
use URI::Escape;
use URI::QueryParam;
use HTML::FromANSI;
use Data::Printer;
use XML::LibXML;

use Log::Log4perl qw(:easy);
my $logfile = dirname($0) . q(/hooks_log4perl.conf);
Log::Log4perl::init_and_watch( $logfile, 10 );
my $logger = Log::Log4perl->get_logger(q(hooks));

#==============================================================================
# LOCAL FUNCTIONS
#==============================================================================

sub url2filename {
    my $url = shift;

    my $uri    = URI->new($url);
    my $scheme = $uri->scheme();
    my $path   = $uri->path();

    return unless $scheme;

    return uri_unescape($path) if $scheme eq q(file);

    my $filename   = uri_escape($url);
    my $suffix     = q(.html);
    my $max_length = 250 - length $suffix;

    $filename = ( substr $filename, 0, $max_length ) . $suffix;

    my $cachefile = "$ENV{ELINKS_CACHE}/$filename";

    return $cachefile;
} ## end sub url2filename

sub cache_file {
    my $url  = shift();
    my $html = shift();

    my $cachefile = &url2filename($url);

    if ($cachefile) {
        if ( index( $cachefile, $ENV{ELINKS_CACHE} ) == 0 ) {
            open my $cachefh, q(>), $cachefile
                or $logger->error(
                qq(ERROR: cannot write file "$cachefile": $!\n));
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

    cache_file( $url, $html );

    foreach ($html) { s{\r\n}{\n}gmsx; }

    my $uri = URI->new($url);

    # if (   $uri->host() eq q(192.168.1.1)
    #     && $uri->path() eq q(/api/monitoring/status) )
    # {
    #     my $xml = XML::LibXML->new->parse_string($html);
    #     my %xml;
    #     foreach my $node ( $xml->findnodes(q{/response/node()}) ) {
    #         my $key   = $node->findvalue(q{name(.)});
    #         my $value = $node->findvalue(q{string(text())});
    #         if ( grep { !(defined) || $_ eq q() } ( $key, $value ) ) {
    #             next;
    #         }
    #         $xml{$key} = $value;
    #     }

    #     $html = ansi2html( p( %xml, colored => 1 ) );
    # } ## end if ( $uri->host() eq q(192.168.1.1)...)
    # els
        if ( $uri->host() eq q(br-linux.org) ) {
        my $pattern = qr{<meta\scharset="utf-8">};
        my $replace
            = qq{<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />};
        $html =~ s{$pattern}{$replace}imsx;
    }

    return $html;
} ## end sub pre_format_html_hook

sub goto_url_hook {
    my $cur_url = $_[0];
    my $new_url;

    my $f = sub {
        my $path = File::Spec->catfile(@_);
        my $file = ( grep {-e} glob $path )[0];
        return $file;
    };

    my %goto_url = (
        q(aria2c)           => q(/usr/doc/aria2-*/README.html),
        q(asciidoc)         => q(/usr/doc/asciidoc-*/doc/),
        q(autoconf)         => q(/usr/doc/autoconf-*/),
        q(bash)             => q(/usr/doc/bash-*/),
        q(bc)               => q(/usr/doc/bc-*/),
        q(bind)             => q(/usr/doc/bind-*/arm/Bv*ARM.html),
        q(bitlbee)          => q(/usr/doc/bitlbee-*/),
        q(bridge-utils)     => q(/usr/doc/bridge-utils-*/doc/),
        q(bzip2)            => q(/usr/doc/bzip2-*/manual.html),
        q(clisp)            => q(/usr/doc/clisp-*/doc/),
        q(ctags)            => q(/usr/doc/ctags-*/),
        q(cups)             => q(/usr/doc/cups-*/index.html),
        q(curl)             => q(/usr/doc/curl-*/),
        q(curl.manual)      => q(/usr/doc/curl-*/MANUAL),
        q(cyrus-sasl)       => q(/usr/doc/cyrus-sasl-*/doc/index.html),
        q(dc3dd)            => q(/usr/doc/dc3dd-*/),
        q(cron)             => q(/usr/doc/dcron-*/),
        q(dhcp)             => q(/usr/doc/dhcp-*/),
        q(dnsmasqs)         => q(/usr/doc/dnsmasq-*/),
        q(docbook-utils)    => q(/usr/doc/docbook-utils-*/html/index.html),
        q(docboolk-xsl)     => q(/usr/doc/docbook-xsl-*/index.html),
        q(doxygen)          => q(/usr/doc/doxygen-*/html/index.html),
        q(dvd+rw-tools)     => q(/usr/doc/dvd+rw-tools-*/index.html),
        q(ed)               => q(/usr/doc/ed-*/),
        q(elinks)           => q(/usr/doc/elinks-*/doc/),
        q(elm)              => q(/usr/doc/elm-*/),
        q(enscript)         => q(/usr/doc/enscript-*/FAQ.html),
        q(espeak)           => q(/usr/doc/espeak-*/index.html),
        q(expat)            => q(/usr/doc/expat-*/reference.html),
        q(expect)           => q(/usr/doc/expect-*/),
        q(fetchmail)        => q(/usr/doc/fetchmail-*/),
        q(flac)             => q(/usr/doc/flac-*/index.html),
        q(gc)               => q(/usr/doc/gc-*/),
        q(git)              => q(/usr/doc/git-*/Documentation/),
        q(hal)              => q(/usr/doc/hal-*/spec/hal-spec.html),
        q(imagemagick)      => q(/usr/doc/ImageMagick-*/index.html),
        q(iproute2)         => q(/usr/doc/iproute2-*/),
        q(iptraf)           => q(/usr/doc/iptraf-*/),
        q(iptraf.manual)    => q(/usr/doc/iptraf-*/Documentation/manual.html),
        q(kbd)              => q(/usr/doc/kbd-*/kbd.FAQ.html),
        q(libnet)           => q(/usr/doc/libnet-*/),
        q(libxml2)          => q(/usr/doc/libxml2-*/),
        q(libxml2.manual)   => q(/usr/doc/libxml2-*/html/html/index.html),
        q(libxml2.tutorial) => q(/usr/doc/libxml2-*/html/tutorial/index.html),
        q(libxslt)          => q(/usr/doc/libxslt-*/html/index.html),
        q(lilo)             => q(/usr/doc/lilo-*/),
        q(linux-faqs)   => q(/usr/doc/Linux-FAQs/Threads-FAQ/html/index.html),
        q(linux-howtos) => q(/usr/doc/Linux-HOWTOs/INDEX.html),
        q(linuxdoc-tools) => q(/usr/doc/linuxdoc-tools-*/html/guide.html),
        q(lua)            => q(/usr/doc/lua-*/html/readme.html),
        q(m4)             => q(/usr/doc/m4-*/),
        q(mpg123)         => q(/usr/doc/mpg123-*/),
        q(mplayer)        => q(/usr/doc/MPlayer-*/HTML/en/MPlayer.html),
        q(mutt)           => q(/usr/doc/mutt/),
        q(mutt.manual)    => q(/usr/doc/mutt/manual.txt),
        q(muttng)         => q(/usr/doc/muttng-*/),
        q(muttng.manual)  => q(/usr/doc/muttng-*/html/index.html),
        q(mysql)          => q(/usr/doc/mysql-*/),
        q(nc)             => q(/usr/doc/nc-*/),
        q(netcat)         => q(/usr/doc/nc-*/),
        q(ntp)            => q(/usr/doc/ntp-*/html/index.html),
        q(ntpc)           => q(/usr/doc/ntp-*/html/index.html),
        q(ntpd)           => q(/usr/doc/ntp-*/html/index.html),
        q(ntpdate)        => q(/usr/doc/ntp-*/html/index.html),
        q(openssl)        => q(/usr/doc/openssl-*/),
        q(ssl)            => q(/usr/doc/openssl-*/),
        q(7zip)           => q(/usr/doc/p7zip-*/DOCS/),
        q(p7zip)          => q(/usr/doc/p7zip-*/DOCS/),
        q(pcre)           => q(/usr/doc/pcre-*/html/index.html),
        q(postgis)        => q(/usr/doc/postgis-*/html/postgis.html),
        q(pgsql)          => q(/usr/doc/postgresql-*/html/index.html),
        q(postgresql)     => q(/usr/doc/postgresql-*/html/index.html),
        q(psql)           => q(/usr/doc/postgresql-*/html/index.html),
        q(r)              => q(/usr/doc/R-*/),
        q(rsync)          => q(/usr/doc/rsync-*/),
        q(sendmail)       => q(/usr/doc/sendmail-*/),
        q(sgml-common)    => q(/usr/doc/sgml-common-*/html/index.html),
        q(sgmlspl)        => q(/usr/doc/sgmlspl-*/sgmlspl/sgmlspl.html),
        q(sjeng)          => q(/usr/doc/Sjeng-Free-*/),
        q(subversion)     => q(/usr/doc/subversion-*/),
        q(svn)            => q(/usr/doc/subversion-*/),
        q(sysstat)        => q(/usr/doc/sysstat-*/),
        q(tetex)          => q(/usr/doc/tetex-*/index.html),
        q(tidy)           => q(/usr/doc/tidy-*/htmldoc/),
        q(tig)            => q(/usr/doc/tig-*/),
        q(transfig)       => q(/usr/doc/transfig-*/manual/),
        q(w3m)            => q(/usr/doc/w3m-*/),
        q(wpa_supplicant) => q(/usr/doc/wpa_supplicant-*/),
        q(xz)             => q(/usr/doc/xz-*/),
        q(zsh)            => q(/usr/doc/zsh-*/html-docs/zsh.html),
        q(sqlite)         => qq(ENV{HOME}/usr/doc/sqlite-doc*/),
        q(about:cache)    => qq($ENV{ELINKS_CACHE}/),
        q(slack)          => qq($ENV{HOME}/usr/doc/slackbook/html/index.html),
        q(slackware)      => qq($ENV{HOME}/usr/doc/slackbook/html/index.html),
    );

    if ( my $path = delete $goto_url{ lc $cur_url } ) {
        $new_url = $f->($path);
    }
    elsif ( $cur_url eq q(about:goto_url) ) {
        my $file = File::Spec->catpath( $ENV{TMP_DIR}, q(goto_url.html) );
        open my $fh, q(>), $file;
        my @goto_url
            = sort { lc $goto_url{$a} cmp lc $goto_url{$b} || $a cmp $b }
            keys %goto_url;
        say $fh sprintf q{<title>%s</title>}, $cur_url;
        foreach my $goto_url (@goto_url) {
            my $file = $f->( $goto_url{$goto_url} );
            say $fh sprintf sprintf
                q{<p><a href="%2$s"><b>%1$s</b></a> => <a href="%2$s">%2$s</a>},
                $goto_url, $file;
        }
        close $fh;
        $cur_url = $file;
    } ## end elsif ( $cur_url eq q(about:goto_url))

    $cur_url = $new_url if defined $new_url && $new_url ne q();

    return $cur_url;
} ## end sub goto_url_hook

sub follow_url_hook { my $url = shift; return $url; }

#sub proxy_for_hook {}
#sub quit_hook {}

1;
