" ${XDG_CONFIG_HOME:-${HOME}}/.vimrc
" author: milomouse <vincent[at]fea.st>
" update: 2012-09-29

" ViM + ZSH + UTF-8
set nocompatible
set shell=zsh
set enc=utf-8

" Display
if $DISPLAY =~ ":"
  set t_Co=256
endif
syntax enable
colorscheme cottonmouse
set shortmess=aTItoO
set statusline=[%n]\ %<%f%m%r\ %w\ %y\ \ <%{&fileformat}>%=[%o]\ %l,%c%V\/%L\ \ %P
"if has ('folding')
"  set foldenable
"  set foldmethod=marker
"  set foldmarker={{{,}}}
"  set foldcolumn=0
"endif

" Settings
set nocursorline
set nocursorcolumn
set ignorecase
set smartcase
set smartindent
set noautoindent
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
set completeopt=longest
set splitbelow
set splitright
set wildmode=list:longest,full
set showmode
set showcmd
set list!
set listchars=trail:·,precedes:«,extends:»
set scrolloff=2
set laststatus=2
set ruler
set ttyfast
set nowrap
set noerrorbells
set history=100
set nomodeline
set backup
set backupdir=/tmp/.1009/vim/backups,.
set directory=/tmp/.1009/vim/swapfiles,.
set viminfo='20,<50,s10,h,n~/.vim/viminfo

" Filetypes
set grepprg=grep\ -nH\ $*
autocmd BufNewFile,BufRead /etc/iptables/* set ft=iptables
autocmd BufNewFile,BufRead ~H/rite/mine/* set ft=txt | set wrap | set spell
autocmd BufNewFile,BufRead ~H/mail/*  set ft=mail | set textwidth=72 | set spell
autocmd BufNewFile,BufRead Xdefaults  set ft=xdefaults
autocmd BufNewFile,BufRead .larswmrc  set ft=xdefaults
autocmd BufNewFile,BufRead .stumpwmrc set ft=lisp
autocmd BufNewFile,BufRead ~/zsh/*    set ft=zsh
autocmd BufNewFile,BufRead .TODO_*    set ft=conf
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! 9`\"" |
  \ endif
autocmd FileType html,xml,xsl set spell
autocmd FileType c      set formatoptions+=ro
autocmd FileType make   set noexpandtab shiftwidth=8
autocmd FileType python set expandtab shiftwidth=2 tabstop=2
autocmd FileType c      syn match matchName /\(#define\)\@<= .*/
autocmd FileType cpp    syn match matchName /\(#define\)\@<= .*/
autocmd FileType mail   set tw=64 autoindent expandtab formatoptions=tcqn
autocmd FileType mail   set comments=nb:>
autocmd FileType mail   set list listchars=tab:--,trail:·,precedes:«,extends:»
autocmd FileType mail   vmap D d0[...]^[
autocmd FileType mail   silent normal /--\s*$^M0^[gg/^$^Mj
filetype on
filetype indent off
filetype plugin indent off

" Allowance
let g:GPGUseAgent = 1
let g:netrw_http_cmd = 'w3m'
let g:netrw_http_xcmd = '-dump >'
let g:tex_flavor = "latex"
let g:tex_conceal = 1
let c_space_errors = 1
let c_no_trail_space_error = 1
let c_no_tab_space_error = 1
let java_space_errors = 1
let python_space_errors = 1
let ruby_space_errors = 1

" [un-]Mappings
map <Up> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>
map <End> <NOP>
map <Home> <NOP>
inoremap # X<BS>#
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" Workarounds
autocmd BufWinLeave * call clearmatches() " Fix progressive memory leak on match when switching buffers
