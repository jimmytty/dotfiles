#!/bin/bash

logfile="$HOME/var/log/elinks.log"
#exec 1>> ${logfile} 2>&1

DISPLAY=:1.0
read date time <<< $(date +%F\ %T)

outdir="$HOME/work"
func_name=$1
uri=$2
filename=$($HOME/.elinks/uri2filename.pl $uri)

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
        $pandoc_cmd |         piconv -f utf8 -t latin1
    ) &>> $pandoc_file

    exit 0
}

emacs_w3m () {
    /usr/bin/emacsclient \
        --no-wait \
        --eval '(w3m "'"$uri"'")'
}

$func_name
