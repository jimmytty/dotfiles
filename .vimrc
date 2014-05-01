"Global options
colorscheme desert
set nobackup
set mouse=c
set ruler
set termencoding=latin1

set tabstop=4
set expandtab
set shiftwidth=4
syntax on

map ,i :w<CR>:!aspell -c %<CR>:e<CR>
map ,c :!perl -c %<cr>
map ,t :%!perltidy %<cr>
map ,p :!perlcritic --statistics %<cr>
set formatprg=/usr/bin/par\ 79j

autocmd FileType mail set textwidth=72 formatprg=/usr/bin/par\ 72ijq
autocmd FileType mail :nmap ,i :w<CR>:!aspell -e -c %<CR>:e<CR>
autocmd FileType pod set textwidth=79
au! BufRead,BufNewFile *.json set filetype=json