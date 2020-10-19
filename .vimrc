" Plugins
call plug#begin('~/.vim/plugged')

Plug 'drewtempelmeyer/palenight.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

call plug#end()

" Theme
set background=dark
colorscheme palenight

" Status line
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='deus'
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'

let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.maxlinenr = '㏑'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" Numbering
set number
set relativenumber

" Insert a single char
:nnoremap s :exec "normal i".nr2char(getchar())."\e"<CR>
:nnoremap S :exec "normal a".nr2char(getchar())."\e"<CR>

" Other
set encoding=utf-8
set lines=40 columns=80
set backspace=2
set incsearch
set hlsearch

" Keys
nnoremap <C-F4> :bw<CR>
nnoremap <F6> :buffers<CR>:buffer<Space>
nnoremap <C-S-Tab> :bp<CR>
nnoremap <C-Tab> :bn<CR>

nnoremap <C-A> ggvG$

imap <silent>  ;;;  <C-R>=CommentBlock(input('Enter comment: '), ';')<CR>

" Functions
function! CommentBlock(comment, ...)
    let introducer =  a:0 >= 1  ?  a:1  :  "//"
    let box_char   =  a:0 >= 2  ?  a:2  :  "*"
    let width      =  a:0 >= 3  ?  a:3  :  strlen(a:comment) + 4

    return introducer . " " . repeat(box_char,width) . "\<CR>"
    \    . introducer . " " . box_char . " " . a:comment 
    \        . " " . box_char . "\<CR>"
    \    . introducer . " " . repeat(box_char,width) . "\<CR>"
endfunction
