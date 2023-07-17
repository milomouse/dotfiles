"""""""""""""""""""""""""""""""""""""""""""
""" locate: ${XDG_CONFIG_HOME}/.vimrc
""" author: Vincent (github.com/milomouse)
""" detail: configuration file for `vim'
"""""""""""""""""""""""""""""""""""""""""""

""" Load plugins via `Plug' package manager for ViM
""" [ https://github.com/junegunn/vim-plug > ~/.vim/autoload/plug.vim ]
call plug#begin('~/.vim/plugged')
    Plug 'vim-airline/vim-airline'
    Plug 'lambdalisue/fern.vim'
    Plug 'lambdalisue/fern-hijack.vim'
    Plug 'hrsh7th/fern-mapping-collapse-or-leave.vim'
    " Plug 'Konfekt/FastFold'
    Plug 'plasticboy/vim-markdown'
    Plug 'tomtom/tcomment_vim'
    Plug 'pbogut/fzf-mru.vim'
call plug#end()

""" Airline specific settings
" let g:airline_theme = 'peaksea'
" let g:airline_theme = 'monochrome'
let g:airline_theme = 'candymouse'
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#tab_nr_type= 2
let g:airline#extensions#tabline#show_tab_type = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_sep_alt = ''
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_section_z = airline#section#create(['--%1p%%-- ',
    \ '%#__accent_bold#%l%#__restore__#', ':%c %L'])
let g:airline_mode_map = {
    \ '__' : '--',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V-L',
    \ '' : 'V-B',
    \ 's'  : 'S',
    \ 'S'  : 'S-L',
    \ '' : 'S-B',
    \ 't'  : 'T',
    \ }
:autocmd!
:autocmd VimEnter * :AirlineRefresh

""" Other plugin specific settings
let g:fzf_mru_no_sort = 0
let g:fzf_mru_relative = 0
autocmd FileType fzf tnoremap <buffer> <Esc> <Esc>
map <C-o> :FZF<CR>
map <C-i> :FZF /home<CR>
map <C-p> :FZFMru<CR>
map <C-n> :Fern . -wait<CR>

""" Core ViM settings
set nocompatible
set hidden
set shell=zsh
set encoding=utf-8
if $DISPLAY =~ ":"
    set t_Co=256
endif
set fileformats=unix,dos,mac
syntax enable
colorscheme candymouse

""" Miscellaneous
set number
set cursorline
set nocursorcolumn
set mouse=v
set mousehide
set nofoldenable
set nowrap
set ignorecase
set smartcase
set smarttab
set smartindent
set autoindent
set nocindent
set noincsearch
set nohls
set showmatch
set complete-=i
set completeopt=longest
set list!
set listchars=tab:│·,trail:·,nbsp:¬,precedes:▒,extends:▒
set scrolloff=2
set ttyfast
set noerrorbells
set splitbelow
set splitright
set ruler
set showcmd
set noshowmode
set nomodeline
set shortmess=aTItoOc
set statusline=▒[%n]\ %<%F%m%r\ %w\ %=%y\ <%{&fileformat}>\ --%p%%--\ %l:%c\ %L\ ▒
set laststatus=2
set history=1000
" set noswapfile
set backup
set backupskip+=/tmp/*,/var/tmp/*,/srv/vide/*
set backupdir=~/.vim/backups//,/tmp/vim-backups//
set directory=~/.vim/swapfiles//,/tmp/vim-swapfiles//
set viminfo='20,<50,s10,h,n~/.vim/viminfo

""" Wild/File Globbing
set wildmode=list:longest,full
set wildignore+=*.aux,*.out,*.toc,*.eot,*.otf,*.ttf,*.woff,*.swp,.DS_Store,._*,*.lock,*~
set wildignore+=*.o,*.obj,*.exe,*.dll,*.jar,*.pyc,*.rbc,*.class,*.so
set wildignore+=*.avi,*.m4a,*.mp3,*.oga,*.ogg,*.opus,*.wav,*.webm,*.mp4,*.flac,*.mkv
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.svg,*.jpg,*.jpeg,*.png,*.psd,*.webp,*.xcf
set wildignore+=*.doc,*.docx,*.pdf,*.cbz,*.cbr,*.epub,*.lit,*.mobi,*.azw,*.fb2
set wildignore+=*.zip,*.rar,*.7z,*.tar.gz,*.tar.bz2,*.tar.xz
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*.gem,*.marko.js,*.min.*,*-min.*
set wildignore=*/.git/*,*/.hg/*,*/.svn/*,*/build/*,*/node_modules/*,=npm-debug.log,.sass-cache

" FileType/Indentation
filetype on
filetype indent off
filetype plugin indent off
""" [set default indentation fallback for undefined languages, using spaces only]
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop&
set grepprg=grep\ -nH\ $*
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! 9`\"" |
    \ endif
""" [per FileType local buffer settings]
autocmd FileType markdown setlocal conceallevel=2
""" [style guide]
autocmd FileType c,cpp    setlocal expandtab shiftwidth=4 softtabstop=4 tabstop& formatoptions+=ro cindent
autocmd FileType c,cpp    syn match matchName /\(#define\)\@<= .*/
autocmd FileType crystal  setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType css      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop& formatoptions+=ro
autocmd FileType go       setlocal noexpandtab shiftwidth=4 softtabstop& tabstop=4
autocmd FileType haskell  setlocal expandtab shiftwidth=4 softtabstop=4 tabstop&
autocmd FileType html     setlocal expandtab shiftwidth=2 softtabstop=2 tabstop& formatoptions+=ro
autocmd FileType java     setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType julia    setlocal expandtab shiftwidth=4 softtabstop=4 tabstop&
autocmd FileType json     setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType lisp     setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType lua      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType make     setlocal noexpandtab shiftwidth=8 softtabstop& tabstop=8
autocmd FileType nim      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType python   setlocal expandtab shiftwidth=4 softtabstop=4 tabstop=4
autocmd FileType r        setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType ruby     setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType sh       setlocal expandtab shiftwidth=4 softtabstop=4 tabstop&
autocmd FileType txt      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop& textwidth=105 nospell
autocmd FileType vim      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
autocmd FileType zsh      setlocal expandtab shiftwidth=2 softtabstop=2 tabstop&
""" [unregistered Filename/Extension; assign Syntax]
autocmd BufNewFile,BufRead vifmrc set syntax=vim
autocmd BufNewFile,BufRead *.zsh set syntax=zsh
autocmd BufNewFile,BufRead *.rasi set syntax=config
""" [unregistered Filename/Extension; assign FileType]
" autocmd BufNewFile,BufRead *.twig set filetype=html

""" Allowance/Highlight options per language
let c_space_errors = 1
let c_no_trail_space_error = 1
let c_no_tab_space_error = 1
let go_highlight_extra_types = 1
let go_highlight_fields = 1
let go_highlight_functions = 1
let go_highlight_function_calls = 1
let go_highlight_operators = 1
let go_highlight_types = 0
let java_space_errors = 1
let python_space_errors = 1
let ruby_space_errors = 1

""" Mappings
map gn :bnext<CR>
map gp :bprevious<CR>
map gd :bdelete <CR>
map gb <C-^>
map gl :buffers<CR>:buffer<Space>
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

""" Netrw (prefer the Fern plugin)
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = '30'

""" Performance
autocmd BufWinLeave * call clearmatches()
set lazyredraw
""" [default is 4000ms/4s which leads to noticeable delays]
set updatetime=300
set ttimeoutlen=10
augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
augroup END
