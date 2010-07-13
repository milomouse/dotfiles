syntax enable
set shell=zsh
set enc=utf-8
set t_Co=256
colorscheme cottonmouse
set nocursorline
set nocursorcolumn
set nocompatible
set ignorecase
set smartcase
set noautoindent
set smartindent
set nocindent
set smarttab
set expandtab
set ts=2
set softtabstop=2
set shiftwidth=2
set noincsearch
set nohls
set smartcase
set number
set mouse=v
set mousehide
set showmatch
set splitbelow
set splitright
set showmode
set showcmd
set scrolloff=2
set laststatus=2
set ruler
set ttyfast
set wrap nowrap
set noerrorbells
set history=100
set nomodeline
set backup
set backupdir=~/.vim/backup,.
set viminfo='20,<50,s10,h,n~/.vim/.viminfo
set shortmess=aTItoO
set statusline=[%n]\ %<%f%m%r\ %w\ %y\ \ <%{&fileformat}>%=[%o]\ %l,%c%V\/%L\ \ %P
set grepprg=grep\ -nH\ $*
set list
set listchars=tab:>-,trail:-
inoremap # X<BS>#
autocmd FileType python,sh,bash,zsh,ruby,perl      let StartComment="#"  | let EndComment=""
autocmd FileType lisp,scheme                       let StartComment=";"  | let EndComment=""
autocmd FileType cpp,php,c,javascript              let StartComment="//" | let EndComment=""
autocmd FileType html    let StartComment="<!--" | let EndComment="-->"
autocmd FileType haskell let StartComment="--"   | let EndComment=""
autocmd FileType vim     let StartComment="\""   | let EndComment=""
autocmd BufNewFile,BufRead *muttrc*           set ft=muttrc
autocmd BufNewFile,BufRead *.xcolors          set ft=xdefaults
autocmd BufNewFile,BufRead ~/.mutt/temp/mutt* set ft=mail | set textwidth=72 | set spell
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! 9`\"" |
  \ endif
filetype on
filetype indent off
filetype plugin indent off
let g:GPGUseAgent = 1
let g:tex_flavor = "latex"
let python_space_errors = 1
let ruby_space_errors = 1
let c_space_errors = 1
let c_no_trail_space_error = 1
let c_no_tab_space_error = 1
