;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Time-stamp: <2014-09-02 19:12:36 jimmy>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(desktop-save-mode)
(desktop-read)
(fset 'yes-or-no-p 'y-or-n-p)
(package-initialize)
(ido-mode t)
(appt-activate 1)

(add-to-list 'load-path "~/.emacs.d/modules/")

(add-to-list 'auto-mode-alist '("\\.screen.*\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.xsh\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("rc$" . conf-mode))

(global-set-key (kbd "C-c C-k") 'kill-whole-line)
(global-set-key (kbd "C-x L") 'count-lines-region)

;;; Org-Mode Keys
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; FUNCTIONS Section
(fset 'par
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("h|par 79j" 0 "%d")) arg)))
(global-set-key (kbd "M-p") 'par)

(defun latex-math-preview-ascii ()
  (interactive)
  (shell-command
   (format "%s %s" "brightmare"
           (shell-quote-argument
            (replace-regexp-in-string "[
]+" " "
 (buffer-substring
  (region-beginning)
  (region-end)))))))

(defun espeak()
  (interactive)
  (shell-command-on-region
   (mark)
   (point)
   "/usr/bin/espeak --stdin -s 160 -ven+f2"))

(defun podchecker ()
  (interactive)
  (shell-command-on-region (mark)
               (point) "podchecker"))
(defun zsh-shell ()
  (interactive)
  (term "/bin/zsh")
  (rename-buffer "Zsh"))
;(zsh-shell)

(defun wtf (acronym)
  (interactive "swtf: ")
  (shell-command (concat "wtf" " " acronym)))

(defun quote-postgres (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "piconv -f latin1 -t utf8 | uni2ascii -q -a7" t t))

(defun quote-mongodb (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "piconv -f latin1 -t utf8 | uni2ascii -q -aU" t t))

(add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_example". "#\\+end_example"))

;;; elfeed.el --- an Emacs Atom/RSS feed reader
(require 'elfeed)
(setq elfeed-feeds '("http://sourceforge.net/blog/feed/"))

;;; number.el --- Working with numbers at point.
(require 'number)
(global-set-key (kbd "C-c +") 'number/add)
(global-set-key (kbd "C-c -") 'number/sub)
(global-set-key (kbd "C-c *") 'number/multiply)
(global-set-key (kbd "C-c /") 'number/divide)
(global-set-key (kbd "C-c 0") 'number/pad)
(global-set-key (kbd "C-c =") 'number/eval)

;;; moz.el --- Lets current buffer interact with inferior mozilla.
(require 'moz)

;;; sqlite.el --- use sqlite via elisp
(require 'sqlite)

;;; ack.el --- Use ack where you might usually use grep.
(require 'ack)
(setq ack-command "ack --noenv --color --pager='cat' --sort-files ")

;;; ansi-color.el --- translate ANSI escape sequences into faces
(require 'ansi-color)

;;; apache-mode.el --- major mode for editing Apache configuration files
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;; AUCTeX is  a comprehensive customizable integrated  environment for writing
;;; input  files   for  TeX, LaTeX,  ConTeXt, Texinfo, and  docTeX  using Emacs
;;; or XEmacs.
(load "auctex.el" nil t t)
(require 'texmathp)

;;; aumix-mode.el --- run the aumix program in a buffer
(autoload 'aumix "aumix-mode" nil t)
(setq aumix-mode-program "alsamixer")

;;; awk-it.el --- Little utility that combines awk and yasnippet.
(eval-after-load 'yasnippet '(require 'awk-it))

;;; bc-mode.el --- BC code editing commands for Emacs
(add-to-list 'load-path "~/.emacs.d/el-get/bc-mode/")
(autoload 'bc-mode "bc-mode.el" "bc-mode" t 'nil)
(add-to-list 'auto-mode-alist '(".bc\\'" . bc-mode))
(add-to-list 'interpreter-mode-alist '("bc" . bc-mode))

;;; bitlbee.el --- Help get Bitlbee (http://www.bitlbee.org) up and running.
(require 'bitlbee)

;;; boxquote.el --- Quote text with a semi-box.
(require 'boxquote)

;;; cdlatex.el -- Fast input methods for LaTeX environments and math
(add-to-list 'load-path "~/.emacs.d/el-get/cdlatex-mode/")
(require 'cdlatex)

;;; col-highlight.el --- Highlight the current column.
(add-to-list 'load-path "~/.emacs.d/el-get/col-highlight/")
(eval-after-load 'vline '(require 'col-highlight))

;;; column-marker.el --- Highlight certain character columns
(add-to-list 'load-path "~/.emacs.d/el-get/column-marker/")
(require 'column-marker)

;;; crontab-mode.el --- Mode for editing crontab files
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;;; crosshairs.el --- Highlight the current line and column.
(add-to-list 'load-path "~/.emacs.d/el-get/crosshairs/")
(eval-after-load 'hl-line+ '(require 'crosshairs))

;;; csv-mode.el --- major mode for editing comma-separated value files
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
   "Major mode for editing comma-separated value files." t)

;;; csv-nav.el --- navigate and edit CSV files
(require 'csv-nav)

;;; dired-async.el --- Copy/move/delete asynchronously in dired.
;;(eval-after-load "dired-aux" '(require 'dired-async))

;;; doc-mode.el --- a major-mode for highlighting a hierarchy structured text.
(add-to-list 'load-path "~/.emacs.d/el-get/doc-mode/")
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
;;; asciidoc.el --- asciidoc text file development support
(add-to-list 'load-path "~/.emacs.d/el-get/asciidoc/")
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-hook 'doc-mode-hook
      '(lambda ()
         (turn-on-auto-fill)
         (require 'asciidoc)))

;;; ecmascript-mode.el --- major mode for editing ECMAScript code
(add-to-list 'load-path "~/.emacs.d/el-get/ecmascript-mode/")
(require 'ecmascript-mode)

;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
 (unless (require 'el-get nil t)
   (url-retrieve
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
    (lambda (s)
      (end-of-buffer)
     (eval-print-last-sexp))))

;;; elisp-format.el --- Format elisp code
(add-to-list 'load-path "~/.emacs.d/el-get/elisp-format/")
(require 'elisp-format)

;;; emms.el --- The Emacs Multimedia System
(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-cache-disable)
(emms-volume-mode-minus)
(emms-mode-line-disable)
(setq emms-info-functions (quote (emms-info-cueinfo emms-info-ogginfo emms-info-mp3info)))
(setq emms-info-mp3find-arguments (quote ("-p" "info-filename=%f\\ninfo-artist=%a\\ninfo-title=%t\\ninfo-album=%l\\ninfo-tracknumber=%n\\ninfo-year=%y\\ninfo-genre=%g\\ninfo-comment=%c\\ninfo-playing-time=%S\\ninfo-composer=%{TCOM}\\ninfo-text_by=%{TEXT}\\ninfo-orchestra=%{TPE2}\\ninfo-conductor=%{TPE3}\\ninfo-disk_n=%{TPOS}")))
(setq emms-info-mp3info-program-name "mp3info")
(setq emms-player-list (quote (emms-player-mplayer-playlist emms-player-mplayer)))
(setq emms-player-mplayer-command-name "/usr/bin/mplayer")
(setq emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-vo" "null")))
(setq emms-player-mplayer-playlist-command-name "/usr/bin/mplayer")
(setq emms-playlist-mode-open-playlists nil)

;;; eperiodic.el --- periodic table for Emacs
(add-to-list 'load-path "~/.emacs.d/el-get/eperiodic/")
(require 'eperiodic);

;;; ess.el --- Emacs Speaks Statistics: statistical programming within Emacs
(require 'ess-site)
(autoload 'ess-rdired "ess-rdired" "View *R* objects in a dired-like buffer." t)
(require 'ess-rutils)
(setq ess-directory "~/R/")
(setq ess-history-directory "~/R/")
;;; ess-edit.el --- convenient editing of R code
(add-to-list 'load-path "~/.emacs.d/el-get/ess-edit/")
(require 'ess-edit)
;;; ess-R-data-view.el --- Data viewer for GNU R
(require 'ess-R-data-view)

;;; essh.el --- a set of commands that emulate for bash what ESS is to R.
(add-to-list 'load-path "~/.emacs.d/el-get/essh/")
(require 'essh)
(defun essh-sh-hook ())
(add-hook 'sh-mode-hook 'essh-sh-hook)

;;; fetchmail-mode.el --- Mode for editing .fetchmailrc files
(add-to-list 'load-path "~/.emacs.d/el-get/fetchmail-mode/")
(autoload 'fetchmail-mode "fetchmail-mode.el"
  "Mode for editing .fetchmailrc files" t)
(setq auto-mode-alist (append '(("\..fetchmailrc$" . fetchmail-mode))
                  auto-mode-alist))

;;; figlet.el --- Annoy people with big, ascii art text
(require 'figlet)

;;; get-rfc.el --- Getting and viewing RFCs
(add-to-list 'load-path "~/.emacs.d/el-get/get-rfc/")
(require 'get-rfc)
(setq get-rfc-local-rfc-directory "~/.emacs.d/RFC/")

;;; gnuplot.el -- drive gnuplot from within emacs
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;;; google-translate.el --- Emacs interface to Google Translate
(require 'google-translate)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "pt")
(setq google-translate-enable-ido-completion t)

;;; hl-line+.el --- Extensions to hl-line.el.
(add-to-list 'load-path "~/.emacs.d/el-get/hl-line+/")
(eval-after-load 'col-highlight '(require 'hl-line+))

;;; html-accent.el --- Function that replace accented char by its html representation
(add-to-list 'load-path "~/.emacs.d/el-get/html-accent/")
(autoload 'html-accent "html-accent" "Accent HTML" t)
(autoload 'accent-html "html-accent" "HTML codes to accent" t)

;;; htmlize.el --- Convert buffer text and decorations to HTML.
(require 'htmlize)

;;; indirect-region.el --- act like indirect-buffer but for region
(add-to-list 'load-path "~/.emacs.d/el-get/indirect-region/")
(require 'indirect-region)

;;; info+.el --- Extensions to `info.el'.
(require 'info+)

;;; insert-time-string.el --- Insert the current time.
(add-to-list 'load-path "~/.emacs.d/el-get/insert-time-string/")
(require 'insert-time-string)
(setq insert-time-string-format-alist
      (cons '("pseudo-iso" . "%F %T")
            insert-time-string-format-alist))
(setq insert-time-string-default-format "pseudo-iso")

;;; irfc.el --- Interface for IETF RFC document.
(require 'irfc)
(add-to-list 'auto-mode-alist '("\\rfc[0-9]+\.txt\\'" . irfc-mode))
(setq irfc-assoc-mode t)
(setq irfc-download-base-url "http://tools.ietf.org/rfc/")

;;; ispell-stopwords.el --- use perl POD "=for stopwords" in ispell
(autoload 'ispell-stopwords "ispell-stopwords" nil t)

;;; js2-mode.el --- an improved JavaScript editing mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; json-mode.el --- Major mode for editing JSON files
(require 'json-mode)

;;; kolon-mode.el --- Syntax highlighting for Text::Xslate's Kolon syntax
(require 'kolon-mode)

;;; lang-refactor-perl.el --- Simple refactorings, primarily for Perl
(require 'lang-refactor-perl)

;;; ldap-mode.el --- major modes for editing LDAP schema and LDIF files
(require 'ldap-mode)
(setq ldif-attribute-face (quote default))

;;; ledger-mode.el --- Helper code for use with the "ledger" command-line tool
(require 'ledger-mode)
(setq ledger-use-iso-dates t)
(setq ledger-binary-path "~/usr/bin/ledger")

;;; lua-mode.el --- a major-mode for editing Lua scripts
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; magit.el --- control Git from Emacs
(require 'magit)

;;; mailcap-mode.el --- mailcap file editing mode
(autoload 'mailcap-mode "mailcap-mode" nil t)
(add-to-list 'auto-mode-alist '("/\\.?mailcap\\'" . mailcap-mode))

;;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq markdown-enable-math t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;;; markdown-mode+.el --- extra functions for markdown-mode
(require 'markdown-mode+)

;;; memory-usage.el --- Analyze the memory usage of Emacs in various ways
(require 'memory-usage)

;;; mode-compile.el --- Smart command for compiling files according to major-mode.
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;;; muttrc-mode.el --- Major mode to edit muttrc under Emacs
(add-to-list 'auto-mode-alist '("\\.muttrc\\'"   . muttrc-mode))
(add-to-list 'auto-mode-alist '("\\.muttngrc\\'" . muttrc-mode))
(add-to-list 'auto-mode-alist '("\\abookrc\\'"   . muttrc-mode))
(add-to-list 'auto-mode-alist '("\\.mutt/.*\\'"  . muttrc-mode))
(setq mutt-alias-file-list (quote ("~/.abook/mutt_alias")))

;;; nagios-mode, an Emacs mode for Nagios configuration files.
(add-to-list 'load-path "~/.emacs.d/el-get/nagios-mode/")
(require 'nagios-mode)

;;; nginx-mode.el --- major mode for editing nginx config files
(require 'nginx-mode)

;;; org-panel.el --- Simple routines for us with bad memory
(require 'org-panel)
(eval-after-load 'org-mode 'org-pan)

;;; org-R.el --- Computing and data visualisation in Org-mode using R
(require 'org-R)

;;; orgtbl-sqlinsert.el --- orgtbl to SQL insert statements.
(require 'orgtbl-sqlinsert)

;;; pandoc-mode.el --- Minor mode for interacting with Pandoc
(require 'pandoc-mode)

;;; pcre2el.el --- PCRE/Elisp/rx/SRE regexp syntax converter and utilities
(require 'pcre2el)

;;; perl-pod-coding.el --- coding system from =encoding in perl files
(require 'perl-pod-coding)

;;; perl-pod-gt.el --- helpers for Perl POD <> markup
(autoload 'perl-pod-gt-double "perl-pod-gt" nil t)
(autoload 'perl-pod-gt-single "perl-pod-gt" nil t)
(autoload 'perl-pod-gt-enable "perl-pod-gt")
(add-hook 'perl-mode-hook  'perl-pod-gt-enable)
(add-hook 'cperl-mode-hook 'perl-pod-gt-enable)
(add-hook 'pod-mode-hook   'perl-pod-gt-enable)

;;; perlcritic.el --- minor mode for Perl::Critic integration
(require 'perlcritic)

;;; perlmonks.el --- A simple interface to www.perlmonks.org
;(add-to-list 'load-path "~/.emacs.d/el-get/perlmonks/")
;(require 'perlmonks)

;;; perltidy.el --- Tidy perl code
(add-to-list 'load-path "~/.emacs.d/el-get/perltidy/")
(require 'perltidy)
(eval-after-load "perl-mode"
  '(progn
     (define-key  perl-mode-map (kbd "C-c t") 'perltidy-buffer)))

;;; pod-mode.el --- Major mode for editing .pod-files
(autoload 'pod-mode "pod-mode" "Mode for editing POD files" t)
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-hook 'pod-mode-hook 'font-lock-mode)

;;; proced.el --- operate on system processes like dired
(global-set-key (kbd "C-x p") 'proced)

;;; pst-format.el --- view perl Storable files as human readable text
(require 'pst-format)
(autoload 'pst-format-decode "pst-format")
(add-to-list 'format-alist
     '(pst-format
       "Perl \"Storable\" module data."
       "\\`\\(pst0\\|perl-store\\)"
       pst-format-decode
       pst-format-encode
       t
       nil))

;;; quickrun.el --- Run commands quickly
(add-to-list 'load-path "~/.emacs.d/el-get/quickrun/")
(require 'quickrun)

;;; regex-tool --- A regular expression evaluation tool for programmers
(add-to-list 'load-path "~/.emacs.d/el-get/regex-tool/")
(require 'regex-tool)
(setq regex-tool-backend (quote emacs))

;;; remind-conf-mode.el --- A mode to help configure remind.
(autoload 'remind-conf-mode "remind-conf-mode" "Mode to help with remind files" t)
(add-to-list 'auto-mode-alist '("\\.reminders\\'"        . remind-conf-mode))
(add-to-list 'auto-mode-alist '("\\.remind/.*\\'"        . remind-conf-mode))

;;; rtf-mode.el --- Emacs major mode for viewing/editing raw RTF source
(add-to-list 'load-path "~/.emacs.d/el-get/rtf-mode/")
(require 'rtf-mode)

;;; sdcv.el --- Interface for sdcv (StartDict console version).
(add-to-list 'load-path "~/.emacs.d/el-get/sdcv/")
(eval-after-load 'showtip '(require 'sdcv))

;;; sgml-mode.el --- SGML- and HTML-editing modes
(defun xsh-edit-element (xsh-command)
  "Edit current XML element with XSH"
  (interactive "Mxsh2-command: ")
  (progn
    (sgml-backward-up-element)
    (let ((beg (point))
      (coding-system-for-read 'utf-8)
      (coding-system-for-write 'utf-8))
      (sgml-forward-element)
      (shell-command-on-region beg (point)
                   (concat "xsh2 -qP- " (shell-quote-argument
                             xsh-command) " | sed 1d")
                   t ))))

;;; showtip.el --- Show tip at cursor
(add-to-list 'load-path "~/.emacs.d/el-get/showtip/")
(require 'showtip)

;;; sparql-mode.el - Interactively evaluate SPARQL
(autoload 'sparql-mode "sparql-mode.el"  "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.
(autoload 'sparql-mode "sparql-mode.el" "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;; sqlite-dump.el --- view dump of sqlite database file
(autoload 'sqlite-dump "sqlite-dump" nil t)
(modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))

;;; syslog-mode.el --- Mode for viewing system logfiles
(require 'syslog-mode)
(add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))

;;; tidy.el --- Interface to the HTML Tidy program
(add-to-list 'load-path "~/.emacs.d/el-get/tidy/")
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)

;;; transpar.el --- Transpose paragraph text as a table.
(add-to-list 'load-path "~/.emacs.d/el-get/transpar/")
(require 'transpar)

;;; tsv-mode.el --- Major mode for edit table filesg
(add-to-list 'load-path "~/.emacs.d/el-get/tsv-mode/")
(autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
(autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)
;(add-to-list 'auto-mode-alist '("\\.tsv$" . tsv-mode))

;;; tt-mode.el --- Emacs major mode for editing Template Toolkit files
(autoload 'tt-mode "tt-mode")
(setq auto-mode-alist (append '(("\\.tt$" . tt-mode))  auto-mode-alist))

;;; tty-format.el --- text file backspacing and ANSI SGR as faces
(eval-after-load 'ansi-color  '(require 'tty-format))
(eval-after-load 'tty-format '(add-hook 'find-file-hooks 'tty-format-guess))

;;; txt2tags-mode
(add-to-list 'load-path "~/.emacs.d/el-get/txt2tags-mode/")
(autoload 't2t-mode "txt2tags-mode" "Txt2tags Mode" t)

;;; unaccent.el --- Functions dealing with accented characters.
(add-to-list 'load-path "~/.emacs.d/el-get/unaccent")
(require 'unaccent)
(global-set-key "\M-'" 'unaccent-word)

;;; unfill.el --- The inverse of fill-paragraph and fill-region
(require 'unfill)

;;; vlf.el --- View Large Files
(require 'vlf)
(setq vlf-batch-size 10240)

;;; vline.el --- show vertical line (column highlighting) mode.
(add-to-list 'load-path "~/.emacs.d/el-get/vline/")
(require 'vline)

;;; w3m.el --- an Emacs interface to w3m
(add-to-list 'load-path "~/.emacs.d/el-get/emacs-w3m/")
(require 'w3m-load)
(setq w3m-favicon-default-background "")
(setq w3m-favicon-type nil)
(setq w3m-fb-delete-frame-kill-buffers nil)
(setq w3m-fill-column 79)
(setq w3m-image-default-background "")
(setq w3m-init-file nil)
(setq w3m-new-session-in-background t)
(setq w3m-new-session-url "about:")
(setq w3m-pop-up-windows nil)
(setq w3m-resize-images nil)
(setq w3m-session-show-titles nil)
(setq w3m-show-graphic-icons-in-header-line nil)
(setq w3m-show-graphic-icons-in-mode-line nil)
(setq w3m-tab-track-mouse nil)
(setq w3m-tab-width 4)
(setq w3m-toggle-inline-images-permanently nil)
(setq w3m-toolbar-icon-preferred-image-types nil)
(setq w3m-track-mouse nil)
(setq w3m-treat-image-size nil)
(setq w3m-use-form nil)
(setq w3m-use-tab-menubar nil)

;;; web-mode.el --- major mode for editing html templates
(require 'web-mode)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-heredoc-fontification t)

;;; wget.el --- Interface program of wget on Emacs
(require 'wget)
(setq wget-download-directory "~/work/")

;;; wgrep.el --- Writable grep buffer and apply the changes to files
(add-to-list 'load-path "~/.emacs.d/el-get/wgrep/")
(require 'wgrep)

;;; windmove.el --- directional window-selection routines
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(setq windmove-wrap-around t)

;;; workgroups.el --- workgroups for windows (for Emacs)
(add-to-list 'load-path "~/.emacs.d/el-get/workgroups/")
(require 'workgroups)

;;; xs-mode.el --- A simple major mode for write perl XS code
(add-to-list 'load-path "~/.emacs.d/el-get/xs-mode/")
(autoload 'xs-mode "xs-mode" "Major mode for XS files" t)
(add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))

;;; yaml-mode.el --- Major mode for editing YAML files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; yasnippet.el --- Yet another snippet extension for Emacs.
(add-to-list 'load-path "~/.emacs.d/el-get/yasnippet/")
(require 'yasnippet)

;;; CUSTOMIZE Section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~/usr/info/")))
 '(Man-width 80)
 '(appt-audible nil)
 '(appt-display-diary nil)
 '(appt-display-format (quote window))
 '(before-save-hook (quote (time-stamp)))
 '(browse-url-browser-function (quote w3m-browse-url))
 '(browse-url-generic-program nil)
 '(browse-url-text-browser "lynx")
 '(c-basic-offset 4)
 '(c-default-style (quote ((other . "gnu"))))
 '(calendar-date-style (quote iso))
 '(column-number-mode t)
 '(cperl-highlight-variables-indiscriminately t)
 '(default-justification (quote full))
 '(delete-selection-mode t)
 '(describe-char-unidata-list t)
 '(desktop-load-locked-desktop t)
 '(desktop-save-mode t)
 '(diary-file "~/.diary")
 '(diary-list-entries-hook (quote (diary-include-other-diary-files diary-sort-entries)))
 '(diary-mail-addr "")
 '(diary-mark-entries-hook (quote (diary-mark-included-diary-files)))
 '(dired-listing-switches "-ahl")
 '(erc-autoaway-idle-seconds 1800)
 '(erc-autoaway-message "@4D")
 '(erc-autoaway-mode t)
 '(erc-autojoin-mode t)
 '(erc-fill-mode t)
 '(erc-frame-dedicated-flag t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-log-channels-directory "~/.erc/logs/")
 '(erc-log-insert-log-on-open t)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list match menu netsplit networks noncommands readonly ring stamp track)))
 '(erc-play-sound nil)
 '(erc-port 6667)
 '(erc-server "127.0.0.1")
 '(erc-track-remove-disconnected-buffers t)
 '(fill-column 79)
 '(fill-nobreak-predicate (quote (fill-french-nobreak-p fill-single-word-nobreak-p)))
 '(indent-tabs-mode nil)
 '(latin1-display-face (quote c-annotation-face))
 '(make-backup-files nil)
 '(message-sendmail-extra-arguments (quote ("-a" "lists")))
 '(newsticker-automatically-mark-items-as-old nil)
 '(newsticker-buffer-change-hook nil)
 '(newsticker-date-format "(%Y-%m-%dT%T%z)")
 '(newsticker-debug t)
 '(newsticker-enable-logo-manipulations nil)
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-justification nil)
 '(newsticker-keep-obsolete-items nil)
 '(newsticker-narrow-hook nil)
 '(newsticker-retrieval-interval 86400)
 '(newsticker-retrieval-method (quote extern))
 '(newsticker-scroll-smoothly nil)
 '(newsticker-ticker-interval -1)
 '(newsticker-url-list (quote (("aiyumi" "http://aiyumi.warpstar.net/pt/rss.xml" nil nil nil) ("torproject.blog" "https://blog.torproject.org/blog/feed" nil nil nil) ("perl.blogs" "http://blogs.perl.org/atom.xml" nil nil nil) ("br-linux" "http://br-linux.org/feed.xml" nil nil nil) ("codata.blog" "http://codata.org/blog/feed/" nil nil nil) ("commandlinefu" "http://www.commandlinefu.com/feed/threeup" nil nil nil) ("perl.cpanratings" "http://cpanratings.perl.org/index.rss" nil nil nil) ("effectiveperlprogramming" "http://www.effectiveperlprogramming.com/feed/" nil nil nil) ("emacswiki" "http://www.emacswiki.org/emacs/full.rss" nil nil nil) ("fp2.com.br" "http://fp2.com.br/blog/index.php/feed/" nil nil nil) ("GitHub::JimmyTTY" "https://github.com/jimmytty.atom" nil nil nil) ("hackagenda" "http://www.hackagenda.com.br/feed/" nil nil nil) ("inovacaotecnologica" "http://www.inovacaotecnologica.com.br/boletim/rss.xml" nil nil nil) ("perl.ironman" "http://ironman.enlightenedperl.org/?feed=RSS" nil nil nil) ("masteringemacs" "http://www.masteringemacs.org/feed/" nil nil nil) ("mundoopensource" "http://www.mundoopensource.com.br/feed/" nil nil nil) ("perlhacks" "http://perlhacks.com/feed/" nil nil nil) ("perlmodules.net" "https://www.perlmodules.net/feed/personal/4aXAIS2XByrC7qaA9NV8nuWrHJgEoSeb0QO7TgdbNWjxxMlkB2" nil nil nil) ("perlnews" "http://perlnews.org/feed/" nil nil nil) ("perlsphere" "http://perlsphere.net/rss.xml" nil nil nil) ("postgresql.org" "http://www.postgresql.org/news.rss" nil nil nil) ("perl.prepan" "http://prepan.org/feed" nil nil nil) ("r-bloggers" "http://www.r-bloggers.com/feed/" nil nil nil) ("worg" "http://orgmode.org/w/?p=worg.git;a=atom;opt=--no-merges" nil nil nil) ("revista.espiritolivre" "http://www.revista.espiritolivre.org/feed" nil nil nil) ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil) ("softwarelivre" "http://softwarelivre.org/portal/feed" nil nil nil) ("tecnoveneno" "http://tecnoveneno.blogspot.com/feeds/posts/default" nil nil nil) ("w3af" "http://w3af.org/feed" nil nil nil) ("fug-br" "http://www.fug.com.br/index2.php?option=com_rss&feed=RSS2.0&no_html=1" nil nil nil) ("mineracaodedados" "https://mineracaodedados.wordpress.com/feed/" nil nil nil) ("danielvinciguerra" "http://danielvinciguerra.com/atom.xml" nil nil nil) ("idgnow.news" "http://www.idgnow.com.br/RSS2" nil nil ("--quiet" "--timeout=60" "--tries=3" "--max-redirect=3" "--output-document=-")) ("idgnow.blog" "http://www.idgnow.com.br/RSS2_blog" nil nil ("--quiet" "--timeout=60" "--tries=3" "--max-redirect=3" "--output-document=-")) ("canaltechbr" "http://feeds.feedburner.com/canaltechbr?format=xml" nil nil nil) ("bsdinfo.com.br" "http://www.bsdinfo.com.br/feed/" nil nil nil) ("openscience" "http://www.openscience.org/blog/?feed=rss2" nil nil nil) ("mongodb.blog" "http://blog.mongodb.org/rss" nil nil nil) ("free_software_foundation.news" "http://static.fsf.org/fsforg/rss/news.xml" nil nil nil) ("free_software_foundation.blogs" "http://static.fsf.org/fsforg/rss/blogs.xml" nil nil nil) ("nerdson" "http://hacktoon.com/feed/rss.xml" nil nil nil) ("postgresqlbr.blogspot" "http://postgresqlbr.blogspot.com/feeds/posts/default?alt=rss" nil nil nil) ("neilb.org" "http://neilb.org/atom.xml" nil nil nil) ("bsdnowmobile" "http://feeds.feedburner.com/BsdNowMobile" nil nil nil) ("planet.postgresql.org" "http://planet.postgresql.org/rss20.xml" nil nil nil) ("savepoint.blog.br" "http://savepoint.blog.br/feed/" nil nil nil) ("planet.mongodb.org" "http://planet.mongodb.org/recent.atom" nil nil nil) ("catonmat.net" "http://www.catonmat.net/feed/" nil nil nil) ("planet.gnu.org" "http://planet.gnu.org/rss20.xml" nil nil nil) ("sourceforge.net" "http://sourceforge.net/blog/feed/" nil nil nil) ("nasa.gov" "http://code.nasa.gov/project/feed/" nil nil nil) ("apache.org" "https://blogs.apache.org/foundation/feed/entries/rss" nil nil nil) ("isc.sans.edu" "https://isc.sans.edu/rssfeed_full.xml" nil nil nil) ("social-engineer.org" "http://www.social-engineer.org/feed" nil nil nil) ("imasters" "http://imasters.com.br/feed" nil nil nil) ("under-linux.org" "https://under-linux.org/external.php?do=rss&type=newcontent&sectionid=1&days=120&count=10" nil nil nil) ("sempreupdate.com.br" "http://www.sempreupdate.com.br/feeds/posts/default" nil nil nil) ("rodrigocalado.com.br" "http://www.rodrigocalado.com.br/feed/" nil nil nil) ("agendati.com.br" "http://www.agendati.com.br/feed/" nil nil nil) ("freebsdish.org" "http://blogs.freebsdish.org/feed/" nil nil nil) ("linuxjournal.com" "http://www.linuxjournal.com/node/feed" nil nil nil) ("planet.lisp.org" "http://planet.lisp.org/rss20.xml" nil nil nil) ("planet.emacsen.org" "http://planet.emacsen.org/atom.xml" nil nil nil) ("planet-commandline.org" "http://planet-commandline.org/rss20.xml" nil nil nil) ("dados.gov.br" "http://dados.gov.br/feed/" nil nil nil) ("ceptro.br" "http://www.ceptro.br/CEPTRO/RssCeptro" nil nil nil) ("cert.br" "http://www.cert.br/rss/certbr-rss.xml" nil nil nil) ("w3c.br" "http://www.w3c.br/Home/WebAtom" nil nil nil) ("serpro.gov.br" "https://www.serpro.gov.br/rss-serpro/RSS" nil nil ("--timeout=60" "--tries=3" "--max-redirect=1" "--no-check-certificate" "--output-document=-")) ("perlmaven-br" "http://br.perlmaven.com/atom" nil nil nil) ("snort.org" "http://blog.snort.org/feeds/posts/default" nil nil nil) ("openwall" "http://rss.gmane.org/messages/complete/gmane.comp.security.openwall.announce" nil nil nil) ("kismetwireless" "http://blog.kismetwireless.net/feeds/posts/default?alt=rss" nil nil nil) ("thc-hydra" "https://github.com/vanhauser-thc/thc-hydra/commits/master.atom" nil nil nil) ("scapy" "http://rss.gmane.org/gmane.comp.security.scapy.general" nil nil nil) ("nmap" "http://seclists.org/rss/nmap-announce.rss" nil nil nil) ("pen-test_seclists.org" "http://seclists.org/rss/pen-test.rss" nil nil nil) ("hack-tools.blackploit" "http://hack-tools.blackploit.com/feeds/posts/default?alt=rss" nil nil nil) ("github.com_blog" "https://github.com/blog/all.atom" nil nil nil) ("programmingperl.org" "http://www.programmingperl.org/feed/" nil nil nil) ("opensource.com" "http://opensource.com/rss.xml" nil nil nil) ("okfn.org" "http://blog.okfn.org/feed/" nil nil ("--config=/dev/null" "--quiet" "--max-redirect=1" "--timeout=60" "--tries=3" "--output-document=-")) ("okfn.org.br" "http://br.okfn.org/feed/" nil nil ("--config=/dev/null" "--quiet" "--max-redirect=1" "--timeout=60" "--tries=3" "--output-document=-")) ("zshwiki.org" "http://zshwiki.org/home/feed.php" nil nil nil) ("planetr.stderr.org" "http://planetr.stderr.org/atom.xml" nil nil nil) ("latexcommunitynews" "http://feeds2.feedburner.com/LatexCommunityNews" nil nil nil) ("w3.org" "http://www.w3.org/blog/news/feed/atom" nil nil nil) ("cli-apps.org" "http://cli-apps.org/cli-apps-content.rdf" nil nil nil) ("dicas-l" "http://www.dicas-l.com.br/index.rdf" nil nil nil) ("texample.net" "http://www.texample.net/feeds/community/" nil nil nil) ("latexbr.blogspot.com" "http://latexbr.blogspot.com/feeds/posts/default" nil nil nil) ("vivendoentresimbolos.com" "http://www.vivendoentresimbolos.com/feeds/posts/default" nil nil nil) ("meiobit.com" "http://meiobit.com/feed/" nil nil nil) ("openstreetmap.org" "https://blog.openstreetmap.org/feed/" nil nil nil) ("aprendendoestatisticacomor.blogspot.com" "http://aprendendoestatisticacomor.blogspot.com/feeds/posts/default" nil nil nil) ("pdl-sourceforge.net" "http://sourceforge.net/p/pdl/news/feed.rss" nil nil nil) ("pdl-wiki" "http://sourceforge.net/p/pdl/wiki/Main_Page/feed.rss" nil nil nil) ("remind-wiki" "http://www.roaringpenguin.com/wiki/index.php?title=Special:RecentChanges&feed=atom" nil nil nil) ("infoq.com_br" "http://www.infoq.com/br/feed" nil nil nil) ("cienciaaberta.net" "http://www.cienciaaberta.net/feed/" nil nil nil) ("slackbuilds.org" "http://slackbuilds.org/rss/ChangeLog.rss" nil nil nil) ("xmodulo.com" "http://xmodulo.com/feed" nil nil nil) ("obomprogramador.com" "http://www.obomprogramador.com/feeds/posts/default" nil nil nil) ("r4stats.com" "http://r4stats.com/feed/" nil nil nil) ("bash-hackers.org" "http://wiki.bash-hackers.org/feed.php" nil nil nil) ("vivaolinux.com.br" "http://vivaolinux.com.br/index.rdf" nil nil nil) ("emacsblog.org" "http://emacsblog.org/feed/" nil nil nil) ("quicklisp.org" "http://blog.quicklisp.org/feeds/posts/default" nil nil nil) ("common-lisp.net" "http://common-lisp.net/index.xml" nil nil nil) ("ledger" "https://github.com/ledger/ledger/commits/master.atom" nil nil nil) ("lifehacker" "http://feeds.gawker.com/lifehacker/full" nil nil nil) ("ycombinator.com" "https://news.ycombinator.com/rss" nil nil nil) ("howtogeek.com" "http://feeds.howtogeek.com/HowToGeek" nil nil nil) ("linuxaria.com" "http://linuxaria.com/feed" nil nil nil) ("forensicswiki.org" "http://forensicswiki.org/index.php?title=Special:RecentChanges&feed=atom" nil nil nil) ("scraping.pro" "http://scraping.pro/feed/" nil nil nil) ("ietf.org" "http://tools.ietf.org/html/new-rfcs.rss" nil nil nil) ("squidproxy.wordpress.com" "http://squidproxy.wordpress.com/feed/" nil nil nil) ("fsf.org-directory" "http://directory.fsf.org/wiki?title=Special:RecentChanges&feed=atom" nil nil nil) ("memcached-wiki" "http://code.google.com/feeds/p/memcached/gitchanges/basic?path=/NewStart.wiki&repo=wiki" nil nil nil) ("bitlbee.org" "http://www.bitlbee.org/news.r.html.rss" nil nil nil) ("cyberciti.biz" "http://feeds.cyberciti.biz/Nixcraft-LinuxFreebsdSolarisTipsTricks" nil nil nil) ("kernel.org" "https://www.kernel.org/feeds/all.atom.xml" nil nil nil) ("planet.kernel.org" "http://planet.kernel.org/rss20.xml" nil nil nil) ("linux.com" "http://www.linux.com/rss/feeds.php" nil nil nil) ("ffmpeg.org" "https://ffmpeg.org/main.rss" nil nil nil) ("icewalkers.com" "http://www.icewalkers.com/backend/icewalkers.xml" nil nil nil) ("call-with-hopeless-continuation.blogspot.com" "http://call-with-hopeless-continuation.blogspot.com/feeds/posts/default" nil nil nil) ("revolutionanalytics.com" "http://blog.revolutionanalytics.com/rss.xml" nil nil nil) ("emacs-awesome" "https://github.com/emacs-tw/awesome-emacs/commits/master.atom" nil nil nil) ("01.org" "https://01.org/blogs/rss.xml/all" nil nil nil) ("wikemacs.org" "http://wikemacs.org/index.php?title=Special:RecentChanges&feed=atom" nil nil nil) ("opensourcedelivers" "http://feeds.feedburner.com/opensourcedelivers" nil nil nil) ("toptal.com" "http://www.toptal.com/blog.rss" nil nil nil) ("linuxnewmedia.com.br" "http://www.linuxnewmedia.com.br/rss" nil nil nil) ("rede.tux4.com.br-jimmy" "http://rede.tux4.com.br/index.php?do=/jimmy/rss/" nil nil nil))))
 '(newsticker-url-list-defaults nil)
 '(newsticker-use-full-width nil)
 '(newsticker-wget-arguments (quote ("--quiet" "--max-redirect=1" "--timeout=30" "--tries=1" "--output-document=-")))
 '(newsticker-wget-name "wget")
 '(org-agenda-diary-file (quote diary-file))
 '(org-agenda-files (quote ("~/OFFICE/office.org" "~/org-mode/todo.org" "/home/jimmy/org-mode/computer_science.org" "/home/jimmy/org-mode/database.org" "/home/jimmy/org-mode/emacs.org" "/home/jimmy/org-mode/geo.org" "/home/jimmy/org-mode/home.org" "/home/jimmy/org-mode/javascript.org" "/home/jimmy/org-mode/json.org" "/home/jimmy/org-mode/latex.org" "/home/jimmy/org-mode/misc.org" "/home/jimmy/org-mode/org-mode.org" "/home/jimmy/org-mode/perl.org" "/home/jimmy/org-mode/pim.org" "/home/jimmy/org-mode/r.org" "/home/jimmy/org-mode/sysadmin.org" "/home/jimmy/org-mode/vim.org" "/home/jimmy/org-mode/web.org" "/home/jimmy/org-mode/xml.org" "/home/jimmy/org-mode/xpath.org" "/home/jimmy/org-mode/xsh.org" "/home/jimmy/org-mode/yaml.org")))
 '(org-agenda-text-search-extra-files nil)
 '(org-alphabetical-lists t)
 '(org-attach-method (quote ln))
 '(org-babel-load-languages (quote ((awk . t) (C . t) (R . t) (calc . t) (css . t) (emacs-lisp . t) (fortran . t) (gnuplot . t) (haskell . t) (js . t) (latex . t) (ledger . t) (lisp . t) (maxima . t) (matlab . t) (ocaml . t) (octave . t) (org . t) (perl . t) (python . t) (scheme . t) (screen . t) (sh . t) (sql . t) (sqlite . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-emphasis-regexp-components (quote ("     ('\"{" "-   .,:!?;'\")}\\" "    
" "." 1)))
 '(org-enforce-todo-dependencies t)
 '(org-entities-ascii-explanatory t)
 '(org-export-author-info t)
 '(org-export-babel-evaluate nil)
 '(org-export-creator-info t)
 '(org-export-default-language "pt-BR")
 '(org-export-html-postamble nil)
 '(org-export-html-style-include-default nil)
 '(org-export-html-style-include-scripts nil)
 '(org-export-htmlize-output-type (quote inline-css))
 '(org-export-language-setup (quote (("en" "Author" "Date" "Table of Contents" "Footnotes") ("ca" "Autor" "Data" "&Iacute;ndex" "Peus de p&agrave;gina") ("cs" "Autor" "Datum" "Obsah" "Pozn\341mky pod carou") ("da" "Ophavsmand" "Dato" "Indhold" "Fodnoter") ("de" "Autor" "Datum" "Inhaltsverzeichnis" "Fu&szlig;noten") ("eo" "A&#365;toro" "Dato" "Enhavo" "Piednotoj") ("es" "Autor" "Fecha" "&Iacute;ndice" "Pies de p&aacute;gina") ("fi" "Tekij&auml;" "P&auml;iv&auml;m&auml;&auml;r&auml;" "Sis&auml;llysluettelo" "Alaviitteet") ("fr" "Auteur" "Date" "Table des mati&egrave;res" "Notes de bas de page") ("hu" "Szerz&otilde;" "D&aacute;tum" "Tartalomjegyz&eacute;k" "L&aacute;bjegyzet") ("is" "H&ouml;fundur" "Dagsetning" "Efnisyfirlit" "Aftanm&aacute;lsgreinar") ("it" "Autore" "Data" "Indice" "Note a pi&egrave; di pagina") ("nl" "Auteur" "Datum" "Inhoudsopgave" "Voetnoten") ("no" "Forfatter" "Dato" "Innhold" "Fotnoter") ("nb" "Forfatter" "Dato" "Innhold" "Fotnoter") ("nn" "Forfattar" "Dato" "Innhald" "Fotnotar") ("pl" "Autor" "Data" "Spis tre&sacute;ci" "Przypis") ("sv" "F&ouml;rfattare" "Datum" "Inneh&aring;ll" "Fotnoter") ("pt-BR" "Autor" "Data de criação" "Sumário" "Referências"))))
 '(org-export-latex-classes (quote (("article" "\\documentclass[11pt]{article}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("beamer" "\\documentclass{beamer}" org-beamer-sectioning) ("mycurriculum" "\\documentclass[11pt,a4paper]{article}
\\usepackage[top=1cm,left=2cm,right=2cm,bottom=0.1cm]{geometry}
\\usepackage{setspace}
\\linespread{0.1}
\\usepackage{amsfonts,graphicx}
\\usepackage[pdftex,
            pdfauthor={{{{AUTHOR}}}}~<{{{{EMAIL}}}}>,
            pdftitle={{{{TITLE}}}},
            pdfsubject={{{{DESCRIPTION}}}},
            pdfkeywords={{{{KEYWORDS}}}},
            pdfstartview=FitH,urlcolor=blue,colorlinks=true,bookmarks=true
           ]{hyperref}
\\usepackage[latin1]{inputenc}  % char encoding
\\pagestyle{empty}
\\frenchspacing      % no aditional spaces after periods
\\setlength{\\parskip}{8pt}\\parindent=0pt  % no paragraph indentation
\\renewcommand\\familydefault{\\sfdefault}
[NO-DEFAULT-PACKAGES]" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}")))))
 '(org-export-preserve-breaks t)
 '(org-export-run-in-background t)
 '(org-export-with-LaTeX-fragments (quote verbatim))
 '(org-export-with-fixed-width t)
 '(org-export-with-section-numbers nil)
 '(org-fontify-emphasized-text t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$$" "\\(" "\\["))))
 '(org-hide-emphasis-markers t)
 '(org-highlight-latex-fragments-and-specials t)
 '(org-list-indent-offset 0)
 '(org-pretty-entities nil)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-publish-project-alist (quote (("tecnoveneno" :auto-preamble t :base-directory "~/tecnoveneno/working/org/" :base-extension "org" :body-only t :headline-levels 999 :html-extension "html" :html-preamble t :htmlized-source t :publishing-directory "~/tecnoveneno/working/html/" :publishing-function org-publish-org-to-html :recursive t :section-numbers nil :sub-superscript t :table-of-contents nil))))
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-startup-folded (quote content))
 '(org-use-sub-superscripts (quote {}))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/") ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/"))))
 '(perl-tab-always-indent t)
 '(proced-after-send-signal-hook (quote (proced-revert)))
 '(save-interprogram-paste-before-kill nil)
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "/usr/bin/msmtp")
 '(sentence-end-without-period t)
 '(server-mode t)
 '(set-language-environment-hook (quote (url-set-mime-charset-string)))
 '(sh-other-keywords (quote ((bash sh-append bourne "bye" "logout" "select") (bourne sh-append sh "function") (csh sh-append shell "breaksw" "default" "end" "endif" "endsw" "foreach" "goto" "if" "logout" "onintr" "repeat" "switch" "then" "while") (es "break" "catch" "exec" "exit" "fn" "for" "forever" "fork" "if" "return" "throw" "while") (ksh88 sh-append bourne "select") (rc "break" "case" "exec" "exit" "fn" "for" "if" "in" "return" "switch" "while") (sh sh-append shell "./configure" "7z" "7za" "7zr" "a2p" "a2ping" "a2ps" "aafire" "aainfo" "aalib" "aasavefont" "aatest" "abook" "ac" "acct" "accton" "ack" "acl" "aconnect" "addpart" "adduser" "adjtimex" "adventure" "afm2tfm" "agetty" "agrep" "aleph" "align" "allcm" "allneeded" "alsa-utils" "alsaconf" "alsactl" "alsamixer" "amidi" "amixer" "ange-ftp" "animate" "antiword" "aplay" "aplaymidi" "apropos" "arc" "arch" "arecordmidi" "arithmetic" "arp" "arpd" "arping" "aseqdump" "aseqnet" "ash" "aspell" "at" "atc" "atd" "atq" "atrm" "atrun" "aumix" "autoexpect" "autoinsert" "autopasswd" "autorevert" "b2m" "backgammon" "badblocks" "banner" "base64" "basename" "bash" "batch" "battlestar" "bban" "bcd" "bibtex" "bin" "bind" "blkid" "blockdev" "bootlogd" "bpe" "browse-url" "bs" "bsd-finger" "bsd-games" "bsondump" "buildhash" "bunzip2" "bzcat" "bzdiff" "bzgrep" "bzip2" "bzip2recover" "bzless" "bzmore" "c2ph" "cacademo" "cacafire" "cacaplay" "cacaserver" "cacaview" "caesar" "cal" "calc" "calculator" "calendar" "canfield" "card" "cat" "catdoc" "catppt" "cdda2wav" "cdparanoia" "cdrecord" "cdrtools" "centerim" "cfdisk" "cfscores" "chacl" "chage" "chattr" "chcon" "chcpu" "chfn" "chgpasswd" "chgrp" "chkdupexe" "chmod" "chown" "chpasswd" "chroot" "chrt" "chsh" "chvt" "cksum" "clear" "clisp" "clockdiff" "cm2rem" "cmp" "col" "colcrt" "collateindex.pl" "colrm" "column" "comm" "compare" "compile_et" "composeglyphs" "composite" "config_data" "conjure" "consoletype" "contrab" "convert" "corelist" "coreutils" "countmail" "cp" "cpan" "cpan" "cpan2dist" "cpanm" "cpanp" "cpanp-run-perl" "cpio" "cribbage" "crond" "cryptdir" "csplit" "css" "csv2yaml" "ctags" "ctangle" "ctie" "ctrlaltdel" "cups" "curl" "cut" "cweave" "cytune" "date" "dc3dd" "dcron" "dd" "ddate" "deallocvt" "debugfs" "debugreiserfs" "decryptdir" "deformat-c" "deformat-sh" "delpart" "depmod" "devdump" "devtodo" "df" "dialog" "diff" "diff3" "diffpp" "diffstat" "diffutils" "dig" "dir" "dircolors" "dired" "dirname" "diskcopy" "dislocate" "dmesg" "dmidecode" "dmp" "dns" "doc-view" "docbook2dvi" "docbook2html" "docbook2man" "docbook2pdf" "docbook2ps" "docbook2rtf" "docbook2tex" "docbook2texi" "docbook2txt" "doctor" "done" "dprofpp" "du" "dump-acct" "dump-utmp" "dumpe2fs" "dumpkeys" "dvd+rw-tools" "dvi2fax" "dvicopy" "dvihp" "dvilj" "dvilj2p" "dvilj4" "dvilj4l" "dvipdfm" "dvipdft" "dvipng" "dvips" "dvired" "dvitomp" "dvitype" "dzil" "e2freefrag" "e2fsck" "e2fsprogs" "e2image" "e2pall" "e2undo" "ebb" "egrep" "eject" "elvfmt" "elvis" "elvtags" "emacs" "emacsclient" "enc2xs" "encapsulate" "encode_keychange" "enscript" "env" "epsffit" "epstopdf" "esac" "eshell" "espeak" "etags" "etex" "evim" "expand" "expect" "expectk" "expiry" "expr" "extractres" "factor" "faillog" "fallocate" "false" "faucet" "fbset" "fdformat" "fdisk" "fetchmail" "ffap" "fgconsole" "fgrep" "fi" "figlet" "file" "filefrag" "find" "find-file" "find2perl" "findaffix" "finder" "findfs" "findmnt" "findutils" "finger" "fish" "fixdlsrps" "fixfmps" "fixmacps" "fixnt" "fixproc" "fixps" "fixpsditps" "fixpspps" "fixscribeps" "fixtpps" "fixwfwps" "fixwpps" "fixwwps" "flock" "fmt" "fmtutil" "fmtutil-sys" "fold" "fontinst" "for" "formail" "fortune" "free" "fromdos" "fsck" "fsck.cramfs" "fsck.ext2" "fsck.ext3" "fsck.ext4" "fsck.ext4dev" "fsck.minix" "fsfreeze" "fstab-decode" "fstfind" "fstrim" "ftp-rfc" "funzip" "fuser" "gawk" "gcc" "genl" "getafm" "getfacl" "getkeycodes" "getmail" "getopt" "getpeername" "gftodvi" "gftopk" "gftype" "git" "gnuchess" "gomoku" "gpasswd" "grep" "grep-changelog" "groupadd" "groupdel" "groupmems" "groupmod" "groups" "grpck" "grpconv" "grpunconv" "gsftopk" "gunzip" "gzexe" "gzip" "h2ph" "h2xs" "halt" "hangman" "hdparm" "head" "hexdump" "hose" "host" "hostid" "hostname" "htop" "hunt" "huntd" "hwclock" "ibuffer" "icombine" "iconv" "id" "identify" "ido" "iecset" "ifcfg" "ifconfig" "ifstat" "ijoin" "imagemagick" "img2txt" "import" "in" "includeres" "info" "infocmp" "infokey" "infozip" "init" "initlog" "initscript" "insmod" "insmod.static" "install" "install-catalog" "install-info" "instmodsh" "ionice" "iostat" "ip" "ipcmk" "ipcrm" "ipcs" "ipf-mod.pl" "ipmaddr" "ipmask" "iproute2" "iptables" "iptraf" "iptunnel" "iputils" "isodebug" "isodump" "isoinfo" "isosize" "isovfy" "ispell" "join" "jw" "kbd" "kbd_mode" "kbdrate" "kibitz" "kill" "killall" "killall5" "kpseaccess" "kpsereadlink" "kpsestat" "kpsetool" "kpsewhere" "kpsewhich" "ksh" "last" "last-acct" "lastcomm" "lastlog" "lbdb" "ld" "ldap" "ldapcompare" "ldapdelete" "ldapexop" "ldapmodify" "ldapmodrdn" "ldappasswd" "ldapsearch" "ldapurl" "ldapwhoami" "ldattach" "ledger" "less" "lessecho" "lesskey" "lesspipe.sh" "libcaca" "libnetcfg" "libxml2" "lilo" "line" "link" "links" "linum" "linuxdoc" "linuxdoc-tools" "ln" "lnstat" "loadunimap" "locate" "lockfile" "logger" "login" "logname" "logoutd" "logsave" "longlines" "look" "losetup" "lp" "lpunlock" "ls" "lsattr" "lsblk" "lscpu" "lsdev" "lsmod" "lsof" "lspci" "lsusb" "lua" "lynx" "lzmadec" "lzmainfo" "mag" "mailstat" "mailx" "make" "makeindex" "makeinfo" "makempx" "makewhatis" "man" "man2dvi" "man2html" "mapscrn" "mc" "mcookie" "md5sum" "mech-dump" "mesg" "mf" "mf-nowin" "mft" "mib2c" "mib2c-update" "mii-tool" "mille" "mk_cmds" "mkafmmap" "mkdir" "mke2fs" "mkfifo" "mkfs" "mkfs.bfs" "mkfs.cramfs" "mkfs.minix" "mkindex" "mkisofs" "mklost+found" "mknod" "mkocp" "mkofm" "mkpasswd" "mkreiserfs" "mkswap" "mktemp" "mktemp-gnu" "mktexlsr" "mktexmf" "mktexpk" "mktextfm" "mkzftree" "modinfo" "modprobe" "module-init-tools" "mogrify" "mongo" "mongodump" "mongoexport" "mongoimport" "mongorestore" "mongostat" "mongotop" "monop" "montage" "more" "morse" "most" "mount" "mountpoint" "mpg123" "mplayer" "mpost" "mpstat" "mpto" "msmtp" "multitail" "multixterm" "munchlist" "mutt" "muttprint" "mv" "mysql" "mysqldump" "mysqlimport" "namei" "nameif" "nasm" "nc" "ncat" "ncftp" "ncurses" "ndiff" "net-snmp" "net-snmp-config" "net-snmp-create-v3-user" "net-tools" "net-utils" "netpipes" "netresolv" "netstat" "netwatch" "newer" "newsticker" "newusers" "nice" "nl" "nmap" "nohup" "nologin" "nproc" "nslookup" "nstat" "ntp" "number" "od" "odvicopy" "odvitype" "ogg123" "oggdec" "oggenc" "ogginfo" "ogonkify" "oleo" "omega" "omfonts" "onsgmls" "openjade" "openldap-client" "openssh" "openssl" "openvt" "osgml2xml" "osgmlnorm" "ospam" "ospcat" "ospent" "otangle" "otp2ocp" "outocp" "over" "p7zip" "pandoc" "par" "parallel" "partx" "passmass" "passwd" "paste" "patch" "patgen" "pathchk" "pciutils" "pcre" "pcregrep" "pcretest" "pdfetex" "pdffonts" "pdfimages" "pdfinfo" "pdflatex" "pdftexi2dvi" "pdftoabw" "pdftohtml" "pdftoppm" "pdftops" "pdftotext" "pdfxtex" "pdiff" "perl" "perlbug" "perlcritic" "perldb" "perldoc" "perlinfo" "perlivp" "perlthanks" "perltidy" "pfb2pfa" "pg" "phantasia" "piconv" "pidstat" "pig" "ping" "ping6" "pinky" "pivot_root" "pk2bm" "pkill" "pktogf" "pktype" "pl2pm" "play" "plipconfig" "pltotf" "pmap" "pod2cpanhtml" "pod2html" "pod2latex" "pod2man" "pod2text" "pod2usage" "podchecker" "podselect" "pom" "pooltype" "poppler" "postgresql" "ppt" "pr" "precat" "preunzip" "prezip" "prezip-bin" "primes" "printenv" "printf" "prlimit" "procinfo" "procmail" "procps" "prove" "ps" "ps2frag" "ps2pk" "ps4pdf" "psbook" "psfxtable" "pslatex" "psmandup" "psmerge" "psnup" "psresize" "psselect" "psset" "pstops" "pstree" "ptar" "ptardiff" "ptx" "pwck" "pwconv" "pwunconv" "quiz" "rain" "random" "rarp" "rarpd" "raw" "rawtime" "rcirc" "rcs-checkin" "rdev" "rdisc" "readcd" "readlink" "readprofile" "realpath" "rec" "recentf" "rect.el" "ref" "regex" "reiserfsck" "reiserfsprogs" "reiserfstune" "rem2html" "rem2ps" "remember" "remind" "rename" "renice" "reset" "resize2fs" "resize_reiserfs" "resizecons" "return" "rev" "rexima" "rftp" "rlogin-cwd" "rm" "rmdir" "rmmod" "robots" "rot13" "route" "routef" "routel" "rsync" "rtacct" "rtcwake" "rtf2rtf" "rtmon" "rtpr" "ruler" "rumakeindex" "run-parts" "runcon" "runlevel" "rvnamed" "s2p" "sa" "sadf" "sail" "sar" "savelog" "sc" "scgcheck" "scp" "screen" "script" "scriptreplay" "sdcv" "sdiff" "sed" "sedsed" "seejpeg" "sem" "sendmail" "seq" "setarch" "setconsolefont" "setfacl" "setfont" "setkeycodes" "setleds" "setmetamode" "setpci" "setserial" "setsid" "setterm" "sfdisk" "sftp" "sgml" "sgml2info" "sgml2latex" "sgml2txt" "sgmldiff" "sgmlpre" "sgmlsasp" "sgmlspl" "sgmlwhich" "sha1sum" "sha224sum" "sha256sum" "sha384sum" "sha512sum" "shadow" "shasum" "showchar" "showconsolefont" "showkey" "shred" "shuf" "shutdown" "sjeng" "skel" "skill" "slabtop" "slattach" "sleep" "sliceprint" "slocate" "snake" "snmp" "snmpbulkget" "snmpbulkwalk" "snmpcheck" "snmpconf" "snmpdelta" "snmpdf" "snmpget" "snmpgetnext" "snmpnetstat" "snmpset" "snmpstatus" "snmptable" "snmptest" "snmptranslate" "snmptrap" "snmpusm" "snmpvacm" "snmpwalk" "snscore" "sockdown" "socklist" "sort" "sox" "soxi" "speaker-test" "splain" "split" "sql" "squid" "ss" "ssh" "ssh-add" "ssh-agent" "ssh-copy-id" "ssh-keygen" "ssh-keyscan" "sshd" "stat" "states" "stdbuf" "strace" "strfile" "strings" "stty" "su" "sudo" "sulogin" "sum" "svn" "svnadmin" "svndumpfilter" "svnlook" "svnserve" "svnsync" "svnversion" "swaplabel" "swapon" "switch_root" "sync" "sysctl" "sysstat" "sysvbanner" "sysvinit" "table" "tac" "tail" "tailf" "tangle" "tar" "taskset" "tc" "tclsh" "tcpdump" "tcsh" "teachgammon" "tee" "telnet" "tempfile" "tesseract" "tetex" "tex" "texdoc" "texdoctk" "texexec" "texi2dvi" "texi2dvi4a2ps" "texi2html" "texi2pdf" "texindex" "texinfo" "texlinks" "tftopl" "tic" "tidy" "tie" "tig" "time" "timed-read" "timed-run" "timelimit" "timeout" "tknewsbiff" "tkpasswd" "tload" "tmm" "todos" "toe" "top" "touch" "tput" "tr" "tracepath" "tracepath6" "traceroute" "traceroute6" "tramp" "traptoemail" "tre" "tree" "trek" "true" "truncate" "tryaffix" "tset" "tsort" "ttf2afm" "tty" "tune2fs" "tunelp" "txt2regex" "txt2tags" "ul" "umount" "uname" "unbuffer" "unexpand" "uni2ascii" "unicode_start" "unicode_stop" "uniq" "unlink" "unshare" "unzip" "unzipsfx" "updmap" "updmap-sys" "uptime" "urifind" "urlview" "usb-devices" "usbutils" "useradd" "userdel" "usermod" "users" "usleep" "util-linux-ng" "uuidgen" "vcut" "vdir" "vftovp" "vi" "view" "vim" "vimdiff" "vimtutor" "vipw" "visudo" "vmstat" "volname" "vorbis-tools" "vorbiscomment" "vptovf" "w" "w3m" "wall" "wargames" "watch" "wc" "weather" "weave" "wget" "whatis" "whereis" "which" "whitespace" "who" "whoami" "whois" "windmove" "wipefs" "woman" "word-list-compress" "workbone" "worm" "worms" "write" "wtf" "wump" "wvdial" "xargs" "xkibitz" "xls2csv" "xml" "xml2po" "xmlcatalog" "xmlif" "xmllint" "xmlto" "xpath" "xpstat" "xsh" "xsubpp" "xx" "xxd" "xz" "xzdec" "xzdiff" "xzgrep" "xzless" "xzmore" "yes" "zcat" "zcmp" "zdiff" "zegrep" "zfgrep" "zforce" "zgrep" "zip" "zipcloak" "zipgrep" "zipnote" "zipsplit" "ziptool" "zless" "zmore" "znew" "zsh") (shell "break" "case" "continue" "exec" "exit") (zsh sh-append bash "select" "foreach"))))
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(show-trailing-whitespace t)
 '(sort-fold-case t)
 '(sql-mode-hook (quote (sql-highlight-ansi-keywords)))
 '(tab-always-indent (quote complete))
 '(tab-stop-list (quote (4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)))
 '(tab-width 4)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(track-eol t)
 '(truncate-partial-width-windows nil)
 '(unibyte-display-via-language-environment nil)
 '(uniquify-ask-about-buffer-names-p t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(user-mail-address "jimmy.tty@gmail.com")
 '(woman-cache-filename "~/.wmncach.el")
 '(woman-cache-level 3)
 '(woman-fill-column 79)
 '(yank-pop-change-selection nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background "black" :foreground "cyan"))))
 '(cperl-hash-face ((t (:background "black" :foreground "cyan"))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "red"))))
 '(org-link ((t (:inherit font-lock-doc-face))))
 '(org-verbatim ((t (:foreground "magenta")))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
