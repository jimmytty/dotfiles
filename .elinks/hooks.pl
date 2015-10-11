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

    return undef unless $scheme;
    return $path if $scheme eq q(file);

    my $opaque = $uri->opaque();
    my @path   = File::Spec->splitdir(qq($opaque));
    @path = grep( /./, @path );
    my $filename = join( q(_), $scheme, @path );
    $filename = substr( $filename, 0, 250 ) if ( length($filename) > 250 );
    $filename .= q(.html);
    my $cachefile = qq($ENV{ELINKS_CACHE}/$filename);

    return $cachefile;

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

    foreach ($html) {
        s{\r\n}{\n}gmsx;
    }

    my $uri = URI->new($url);

    if ( first { $uri->host() eq $_ } q(www.gnu.tux4.com.br) ) {
        use HTML::Entities;
        $html = encode_entities( decode_utf8($html), qq(\200-\377) );
    }
    elsif ( first { $uri->host() eq $_ }
        ( q(getninjas.com.br), q(www.getninjas.com.br), ) )
    {
        my $tree = HTML::TreeBuilder::LibXML->new();
        $tree->store_comments();
        $tree->parse($html);

        my @node = (
            q{//div[@class='header-button']},           #
            q{//div[@class='professionals-column']},    #
            q{//div[@class='flash_messages']},          #
            q{//nav[@id='sco-nav'][@class='area']},     #
            q{//div[@id='sidebar']},                    #
            q{//aside[@id='new-sidebar']},              #
        );
        my $xpath = q[(] . join( q(|), @node ) . q[)];
        while ( my $node = $tree->findnodes($xpath)->[0] ) {
            $node->delete();
        }
        $html = $tree->as_HTML();
        $tree->eof();
        $tree = $tree->delete();
    } ## end elsif ( first { $uri->host...})
    elsif ( $uri->host() eq q(metacpan.org)
        && [ $uri->path_segments() ] ~~ [ q(), q(search) ] )
    {
        # EXPERIMENTAL
        # my $tree = HTML::TreeBuilder::LibXML->new();
        # $tree->store_comments();
        # $tree->parse($html);

      # my %result;
      # my $node = $tree->findnodes(q{//div[@class="content search-results"]})
      #     ->[0];
      # my $xpath = q{div[@class="module-result"]};
      # foreach my $result ( $node->findnodes($xpath) ) {
      #     my $datetime = $result->findvalue(q{span[@class="relatize"]});
      #     my $t = Time::Piece->strptime( $datetime, q(%d %b %Y %T %Z) );
      #     push @{ $result{ $t->epoch() } }, $result;
      #     $result->delete();
      # }

        # foreach my $timestamp ( sort { $a <=> $b } keys %result ) {
        #     foreach my $result ( @{ $result{$timestamp} } ) {
        #         $node->unshift_content($result);
        #     }
        # }
        # $html = $tree->as_HTML();
        # $tree->eof();
        # $tree = $tree->delete();
    } ## end elsif ( $uri->host() eq q(metacpan.org)...)
    elsif ( $uri->host() eq q(www.vivaolinux.com.br) ) {

        # ENCODING PROBLEMS
        $html =~ tr{\r}{}d;
        my $tree = HTML::TreeBuilder::LibXML->new();
        $tree->store_comments();
        $tree->parse($html);

        my $xpath = q{//input[@type="button"]
                            [starts-with(@onclick, 'window.location.href=')]
        };

        foreach my $node ( $tree->findnodes($xpath) ) {
            my $text     = $node->findvalue(q{@value});
            my $location = $node->findvalue(
                q{substring-after(@onclick, 'window.location.href=')});
            $location =~ tr{"'}{}d;
            my $a = HTML::Element->new( q(a), href => $location );
            $a->push_content($text);
            $a = HTML::TreeBuilder::LibXML->new_from_content( $a->as_HTML() );
            $node->preinsert( $a->findnodes(q{//a})->[0] );
            $node->delete();
        }

        $html = $tree->as_HTML();
        $html =~ s{\N{NO-BREAK SPACE}}{&nbsp;}gmsx;
        encode_entities( $html, qq(\200-\377) );
        $html =~ s{\N{NO-BREAK SPACE}}{&nbsp;}gmsx;
        $tree->eof();
        $tree = $tree->delete();
    } ## end elsif ( $uri->host() eq q(www.vivaolinux.com.br))

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

#sub follow_url_hook {}
#sub proxy_for_hook {}
#sub quit_hook {}

1;
