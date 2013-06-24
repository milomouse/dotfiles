" Vim color file
" Author:       milomouse
" Maintainer:   bollovan <bollovan@gmail.com>
" Last Change:  30/Aug/2011
" Colourful yet muted
" Made for 256-color console and gui vim

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="vincent"

if ( &t_Co >= 255 || $TERM =~? "256color" ) || has("gui_running")
  hi Normal         cterm=NONE ctermfg=251    ctermbg=233
  hi Normal         gui=NONE   guifg=#c6c6c6  guibg=#121212

  hi Boolean        cterm=NONE ctermfg=135    ctermbg=NONE
  hi Boolean        gui=NONE   guifg=#ae81ff  guibg=NONE

  hi Character      cterm=NONE ctermfg=204    ctermbg=NONE
  hi Character      gui=NONE   guifg=#ff5f87  guibg=NONE

  hi Comment        cterm=NONE ctermfg=243    ctermbg=235
  hi Comment        gui=NONE   guifg=#767676  guibg=#262626

  hi Conditional    cterm=NONE ctermfg=110    ctermbg=NONE
  hi Conditional    gui=NONE   guifg=#87afd7  guibg=NONE

  hi Constant       cterm=bold ctermfg=239    ctermbg=NONE
  hi Constant       gui=bold   guifg=#4e4e4e  guibg=NONE

  hi Cursor         cterm=NONE ctermfg=233    ctermbg=109
  hi Cursor         gui=NONE   guifg=#121212  guibg=#87afaf

  hi CursorLine     cterm=NONE ctermfg=NONE   ctermbg=234
  hi CursorLine     gui=NONE   guifg=NONE     guibg=#1c1c1c

  hi CursorColumn   cterm=NONE ctermfg=NONE   ctermbg=234
  hi CursorColumn   gui=NONE   guifg=NONE     guibg=#1c1c1c

  hi Debug          cterm=NONE ctermfg=181    ctermbg=NONE
  hi Debug          gui=NONE   guifg=#d7afaf  guibg=NONE

  hi Define         cterm=NONE ctermfg=98     ctermbg=NONE
  hi Define         gui=NONE   guifg=#875fd7  guibg=NONE

  hi Delimiter      cterm=NONE ctermfg=109    ctermbg=NONE
  hi Delimiter      gui=NONE   guifg=#87afaf  guibg=NONE

  hi DiffAdd        cterm=NONE ctermfg=66     ctermbg=237
  hi DiffAdd        gui=NONE   guifg=#5f8787  guibg=#3a3a3a

  hi DiffChange     cterm=NONE ctermfg=NONE   ctermbg=236
  hi DiffChange     gui=NONE   ctermfg=NONE   guibg=#303030

  hi DiffRemoved    cterm=NONE ctermfg=167    ctermbg=NONE
  hi DiffRemoved    gui=NONE   guifg=#d7444b  guibg=NONE

  hi DiffText       cterm=NONE ctermfg=217    ctermbg=237
  hi DiffText       gui=NONE   guifg=#ffafaf  guibg=#121212

  hi Directory      cterm=NONE ctermfg=74     ctermbg=NONE
  hi Directory      gui=NONE   guifg=#5FAFD7  guibg=NONE

  hi ErrorMsg       cterm=NONE ctermfg=13     ctermbg=NONE
  hi ErrorMsg       gui=NONE   guifg=#9d6ffd  guibg=NONE

  hi Error          cterm=NONE ctermfg=167    ctermbg=52
  hi Error          gui=NONE   guifg=#e37170  guibg=#432323

  hi Exception      cterm=NONE ctermfg=249    ctermbg=NONE
  hi Exception      gui=NONE   guifg=#b2b2b2  guibg=NONE

  hi Float          cterm=NONE ctermfg=251    ctermbg=NONE
  hi Float          gui=NONE   guifg=#c6c6c6  guibg=NONE

  hi FoldColumn     cterm=NONE ctermfg=97     ctermbg=238
  hi FoldColumn     gui=NONE   guifg=#5787af  guibg=#444444

  hi Folded         cterm=NONE ctermfg=67     ctermbg=16
  hi Folded         gui=NONE   guifg=#5787af  guibg=#000000

  hi Function       cterm=NONE ctermfg=95     ctermbg=NONE
  hi Function       gui=NONE   guifg=#875f5f  guibg=NONE

  hi Identifier     cterm=NONE ctermfg=110    ctermbg=NONE
  hi Identifier     gui=NONE   guifg=#87afd7  guibg=NONE

  hi IncSearch      cterm=NONE ctermfg=232    ctermbg=214
  hi IncSearch      gui=NONE   guifg=#080808  guibg=#ffaf00

  hi Keyword        cterm=bold ctermfg=140    ctermbg=NONE
  hi Keyword        gui=bold   guifg=#af87d7  guibg=NONE

  hi Label          cterm=NONE ctermfg=187    ctermbg=NONE
  hi Label          gui=NONE   guifg=#d7d7af  guibg=NONE

  hi LineNr         cterm=NONE ctermfg=240    ctermbg=233
  hi LineNr         gui=NONE   guifg=#585858  guibg=#121212

  hi Macro          cterm=NONE ctermfg=98     ctermbg=NONE
  hi Macro          gui=NONE   guifg=#875fd7  guibg=NONE

  hi ModeMsg        cterm=NONE ctermfg=99     ctermbg=NONE
  hi ModeMsg        gui=NONE   guifg=#875fff  guibg=NONE

  hi MoreMsg        cterm=NONE ctermfg=254    ctermbg=NONE
  hi MoreMsg        gui=NONE   guifg=#f7f7f1  guibg=NONE

  hi NonText        cterm=NONE ctermfg=238    ctermbg=234
  hi NonText        gui=NONE   guifg=#444444  guibg=#1c1c1c

  hi Number         cterm=NONE ctermfg=141    ctermbg=NONE
  hi Number         gui=NONE   guifg=#af87ff  guibg=NONE

  hi Operator       cterm=NONE ctermfg=38     ctermbg=NONE
  hi Operator       gui=NONE   guifg=#00afd7  guibg=NONE

  hi PreCondit      cterm=NONE ctermfg=180    ctermbg=NONE
  hi PreCondit      gui=NONE   guifg=#d7af5f  guibg=NONE

  hi Pmenu          cterm=NONE ctermfg=230    ctermbg=238
  hi Pmenu          gui=NONE   guifg=#ffffd7  guibg=#444444

  hi PmenuSel       cterm=NONE ctermfg=232    ctermbg=192
  hi PmenuSel       gui=NONE   guifg=#080808  guibg=#d7ff87

  hi PreProc        cterm=NONE ctermfg=108    ctermbg=NONE
  hi PreProc        gui=NONE   guifg=#87af87  guibg=NONE

  hi Question       cterm=NONE ctermfg=37     ctermbg=NONE
  hi Question       gui=NONE   guifg=#00afaf  guibg=NONE

  hi Repeat         cterm=NONE ctermfg=131    ctermbg=NONE
  hi Repeat         gui=NONE   guifg=#af5f5f  guibg=NONE

  hi Search         cterm=NONE ctermfg=232    ctermbg=103
  hi Search         gui=NONE   guifg=#080808  guibg=#8787af

  hi SpecialChar    cterm=NONE ctermfg=181    ctermbg=NONE
  hi SpecialChar    gui=NONE   guifg=#d7afaf  guibg=NONE

  hi SpecialComment cterm=NONE ctermfg=108    ctermbg=NONE
  hi SpecialComment gui=NONE   guifg=#87af87  guibg=NONE

  hi Special        cterm=NONE ctermfg=216    ctermbg=NONE
  hi Special        gui=NONE   guifg=#efaf7f  guibg=NONE

  hi SpecialKey     cterm=NONE ctermfg=236    ctermbg=NONE
  hi SpecialKey     gui=NONE   guifg=#303030  guibg=NONE

  hi Statement      cterm=NONE ctermfg=103    ctermbg=NONE
  hi Statement      gui=NONE   guifg=#8787af  guibg=NONE

  hi StatusLineNC   cterm=NONE ctermfg=234    ctermbg=59
  hi StatusLineNC   gui=NONE   guifg=#1c1c1c  guibg=#5f5f5f

  hi StatusLine     cterm=bold ctermfg=103    ctermbg=235
  hi StatusLine     gui=bold   guifg=#8787af  guibg=#262626

  hi StorageClass   cterm=NONE ctermfg=249    ctermbg=NONE
  hi StorageClass   gui=NONE   guifg=#b2b2b2  guibg=NONE

  hi String         cterm=NONE ctermfg=174    ctermbg=NONE
  hi String         gui=NONE   guifg=#d78787  guibg=NONE

  hi Structure      cterm=NONE ctermfg=229    ctermbg=NONE
  hi Structure      gui=NONE   guifg=#ffffaf  guibg=NONE

  hi Tag            cterm=NONE ctermfg=155    ctermbg=NONE
  hi Tag            gui=NONE   guifg=#afff5f  guibg=NONE

  hi Title          cterm=NONE ctermfg=251    ctermbg=NONE
  hi Title          gui=NONE   guifg=#c6c6c6  guibg=NONE

  hi Todo           cterm=bold ctermfg=95     ctermbg=235
  hi Todo           gui=bold   guifg=#875f5f  guibg=#262626

  hi Typedef        cterm=NONE ctermfg=253    ctermbg=NONE
  hi Typedef        gui=NONE   guifg=#dadada  guibg=NONE

  hi Type           cterm=NONE ctermfg=102    ctermbg=NONE
  hi Type           gui=NONE   guifg=#878787  guibg=NONE

  hi Underlined     cterm=NONE ctermfg=32     ctermbg=NONE
  hi Underlined     gui=NONE   guifg=#0087af  guibg=NONE

  hi VertSplit      cterm=NONE ctermfg=236    ctermbg=59
  hi VertSplit      gui=NONE   guifg=#303030  guibg=#5f5f5f

  hi WarningMsg     cterm=NONE ctermfg=161    ctermbg=NONE
  hi WarningMsg     gui=NONE   guifg=#d7005f  guibg=NONE

  hi WildMenu       cterm=NONE ctermfg=194    ctermbg=NONE
  hi WildMenu       gui=NONE   guifg=#d7ffd7  guibg=NONE
endif
