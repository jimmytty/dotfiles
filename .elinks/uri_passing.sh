#!/bin/bash

logfile="$HOME/var/log/elinks.log"
exec 1>> ${logfile} 2>&1

DISPLAY=:1.0
read date time <<< $(date +%F\ %T)

outdir="$HOME/work"
func_name=$1
uri=$2
filename=$($HOME/.elinks/uri2filename.pl $uri)
uri_escaped=$(perl -MURI::Escape -wE'print uri_escape shift' "$uri")
title="$(xsh \
            --no-validation \
            --html \
            --input "$filename" \
            --command 'print normalize-space(string(/html/head/title))' \
            2>/dev/null)"
title_escaped=$(perl -MURI::Escape -wE'print uri_escape shift' "$title")
description="$(xsh \
            --no-validation \
            --html \
            --input "$filename" \
            --command 'print /html/head/meta[@content][@name="description" or @itemprop="description"]/@content' \
            2>/dev/null)"
description_escaped=$(perl -MURI::Escape -wE'print uri_escape shift' "$description")

html2org () {
    xsh_src=$(cat <<'EOF'
quiet;
nodebug;
recovering 1;
encoding 'utf-8';
query-encoding 'utf-8';
foreach my $anchor_node in //node()[@id or @name] {
    cd $anchor_node;
    if ( local-name() = 'html' ) { next; }
    my $anchor_name;
    if ( @id ) { $anchor_name = @id; }
    elsif ( @name ) { $anchor_name = @name; }
    my $orgid = concat('<<','#',$anchor_name,'>>');
    insert text $orgid before .;
}
print xsh:serialize(/);
EOF
)
    pandoc_file="$outdir/pandoc_html2org.org"
    pandoc_cmd=$(cat <<'EOF'
pandoc \
    --no-wrap \
    --base-header-level=2 \
    --self-contained \
    --reference-links \
    --from=html \
    --to=org
EOF
)
    (   echo "* $uri";
        xsh \
            --no-validation \
            --html \
            --input "$filename" \
            --command "$xsh_src" |
        $pandoc_cmd | piconv -f utf8 -t latin1 --perlqq
    ) &>> $pandoc_file

    exit 0
}

emacs_w3m () {
    /usr/bin/emacsclient \
        --no-wait \
        --eval '(w3m "'"$uri"'")'
}

emacs_eww () {
    /usr/bin/emacsclient \
        --no-wait \
        --eval '(eww "'"$uri"'")'
}

org_capture () {
    emacsclient \
        --no-wait \
        'org-protocol://capture?'url=$uri_escaped'&'title=$title_escaped'&'body=$description_escaped &
    screen -X select emacs
}

$func_name
