set nocompatible                " choose no compatibility with legacy vi
syntax enable
set encoding=utf-8
set showcmd                     " display incomplete commands
filetype plugin indent on       " load file type plugins + indentation
"set modeline
set ls=2                        " show status line always
set ruler                       " show ruler always

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
"match ExtraWhitespace /\s\+$/
"autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
"if version >= 702
"    autocmd BufWinLeave * call clearmatches()
"endif
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

"" Solarized
set background=dark
let g:solarized_termtrans = 1
colorscheme solarized
