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

"" Solarized
set background=light
let g:solarized_termtrans = 1
colorscheme solarized
