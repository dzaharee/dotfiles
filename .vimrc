""" pathogen
execute pathogen#infect()
call pathogen#helptags()

set nocompatible                " choose no compatibility with legacy vi
syntax enable
set encoding=utf-8
set showcmd                     " display incomplete commands
filetype plugin indent on       " load file type plugins + indentation
set ls=2                        " show status line always
set ruler                       " show ruler always
"set hidden                      " don't close buffers that are abandoned
set history=1000                " keep more history
set wildmode=list:longest       " command tab completion more like bash
set scrolloff=3                 " scroll before we hit the bottom

"" set screen/xterm title
autocmd BufEnter * let &titlestring = "vim " . expand("%:t")
if &term == "screen"
    set t_ts=k
    set t_fs=\
endif
if &term == "screen" || &term == "xterm"
    set title
endif

"" Whitespace
set tabstop=4 shiftwidth=4      " a tab is two spaces (or set this to 4)
set expandtab                   " use spaces, not tabs (optional)
set backspace=indent,eol,start  " backspace through everything in insert mode

"" Searching
set nohlsearch                  " don't highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter

"" Folding
set foldmethod=indent           " fold using indents
set foldlevelstart=20           " start unfolded

"" show bad white space
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd Syntax * syn match ExtraWhitespace /\s\+$/ containedin=ALL
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

"" Add syntax highlighting for .cljx
autocmd BufNewFile,BufReadPost *.cljx setfiletype clojure

"" Solarized
set background=light
let g:solarized_termtrans = 1
colorscheme solarized


"""""""""
"" conque
""""""""

" conque options
let g:ConqueTerm_FastMode = 1       " don't know if we actually care, but lets do this
let g:ConqueTerm_ReadUnfocused = 1  " we want this so we can see our repl is ready

" get clojure syntax in our new repl
function! ConqueStartup(term)
    if a:term.command == 'lein repl :connect' || a:term.command == 'lein repl'
        setlocal syntax=clojure
        syntax clear ExtraWhitespace            " turn of end of line highlighting for the repl
    endif
endfunction
call conque_term#register_function('after_startup', 'ConqueStartup')

" put us where we want to be when we enter the repl buffer (we need this the
" way we're opening it)
function! ConqueEnter(term)
    normal! G$
endfunction
call conque_term#register_function('buffer_enter', 'ConqueEnter')

"" connect to a repl using conque and lein repl with :Repl
function! Repl()
    let replportfile = findfile('.nrepl-port', ';')
    if empty(replportfile)
        let replcommand = 'lein repl'
    else
        let replcommand = 'lein repl :connect'
    endif
    " I like the repl always appearing to the right -dzaharee
    let repl = conque_term#open(replcommand, ['botright vsplit'], 1)
endfunction
command Repl call Repl()
