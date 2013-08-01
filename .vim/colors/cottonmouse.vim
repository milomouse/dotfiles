" Vim color file
" Author:       milomouse <vincent[at]fea.st>
" Maintainer:   milomouse <vincent[at]fea.st>
" Created:      2010-xx-xx
" Last Change:  2013-08-01
" Colorful yet muted
" Designed for 256-color console

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name="cottonmouse"

if ( &t_Co >= 255 || $TERM =~? "256color" )
  hi Normal         term=none cterm=none ctermfg=59   ctermbg=233
  hi Boolean        term=none cterm=none ctermfg=132  ctermbg=none
  hi Character      term=none cterm=none ctermfg=60   ctermbg=none
  hi Comment        term=none cterm=none ctermfg=238  ctermbg=234
  hi Conditional    term=none cterm=none ctermfg=140  ctermbg=none
  hi Constant       term=none cterm=none ctermfg=60   ctermbg=none
  hi Cursor         term=none cterm=none ctermfg=232  ctermbg=109
  hi CursorLine     term=none cterm=none ctermfg=none ctermbg=234
  hi CursorLineNr   term=none cterm=none ctermfg=132  ctermbg=234
  hi CursorColumn   term=none cterm=none ctermfg=none ctermbg=234
  hi Debug          term=none cterm=none ctermfg=181  ctermbg=none
  hi Define         term=none cterm=none ctermfg=97   ctermbg=none
  hi Delimiter      term=none cterm=none ctermfg=73   ctermbg=none
  hi DiffAdd        term=none cterm=none ctermfg=66   ctermbg=236
  hi DiffChange     term=none cterm=none              ctermbg=236
  hi DiffDelete     term=none cterm=none ctermfg=235  ctermbg=237
  hi DiffText       term=none cterm=none ctermfg=217  ctermbg=238
  hi Directory      term=none cterm=none ctermfg=188  ctermbg=none
  hi ErrorMsg       term=none cterm=none ctermfg=38   ctermbg=none
  hi Error          term=none cterm=none ctermfg=108  ctermbg=234
  hi Exception      term=none cterm=none ctermfg=249  ctermbg=none
  hi Float          term=none cterm=none ctermfg=251  ctermbg=none
  hi FoldColumn     term=none cterm=none ctermfg=97   ctermbg=235
  hi Folded         term=none cterm=none ctermfg=65   ctermbg=none
  hi Function       term=none cterm=none ctermfg=132  ctermbg=none
  hi Identifier     term=none cterm=none ctermfg=110  ctermbg=none
  hi IncSearch      term=none cterm=none ctermfg=232  ctermbg=78
  hi Keyword        term=none cterm=none ctermfg=97   ctermbg=none
  hi Label          term=none cterm=none ctermfg=187  ctermbg=none
  hi LineNr         term=none cterm=none ctermfg=235  ctermbg=233
  hi MatchParen     term=none cterm=none ctermfg=235  ctermbg=132
  hi Macro          term=none cterm=none ctermfg=99   ctermbg=none
  hi ModeMsg        term=none cterm=none ctermfg=132  ctermbg=none
  hi MoreMsg        term=none cterm=none ctermfg=60   ctermbg=none
  hi NonText        term=none cterm=none ctermfg=237  ctermbg=234
  hi Number         term=none cterm=none ctermfg=110  ctermbg=none
  hi Operator       term=none cterm=none ctermfg=38   ctermbg=none
  hi PreCondit      term=none cterm=none ctermfg=180  ctermbg=none
  hi PreProc        term=none cterm=none ctermfg=245  ctermbg=none
  hi Question       term=none cterm=none ctermfg=15   ctermbg=none
  hi Repeat         term=none cterm=none ctermfg=97   ctermbg=none
  hi Search         term=none cterm=none ctermfg=75   ctermbg=none
  hi SpecialChar    term=none cterm=none ctermfg=108  ctermbg=none
  hi SpecialComment term=none cterm=none ctermfg=108  ctermbg=none
  hi Special        term=none cterm=none ctermfg=140  ctermbg=none
  hi SpecialKey     term=none cterm=none ctermfg=236  ctermbg=none
  hi Statement      term=none cterm=none ctermfg=139  ctermbg=none
  hi StatusLine     term=none cterm=none ctermfg=234  ctermbg=240
  hi StatusLineNC   term=none cterm=none ctermfg=239  ctermbg=235
  hi StorageClass   term=none cterm=none ctermfg=249  ctermbg=none
  hi String         term=none cterm=none ctermfg=249  ctermbg=none
  hi Structure      term=none cterm=none ctermfg=229  ctermbg=none
  hi TabLine        term=none cterm=none ctermfg=238  ctermbg=235
  hi TabLineFill    term=none cterm=none ctermfg=238  ctermbg=235
  hi TabLineSel     term=none cterm=none ctermfg=243  ctermbg=233
  hi Tag            term=none cterm=none ctermfg=108  ctermbg=none
  hi Title          term=none cterm=none ctermfg=110  ctermbg=none
  hi Todo           term=none cterm=none ctermfg=60   ctermbg=235
  hi Typedef        term=none cterm=none ctermfg=253  ctermbg=none
  hi Type           term=none cterm=none ctermfg=60   ctermbg=none
  hi Underlined     term=none cterm=none ctermfg=188  ctermbg=none
  hi VertSplit      term=none cterm=none ctermfg=234  ctermbg=235
  hi VisualNOS      term=none cterm=none ctermfg=99   ctermbg=none
  hi WarningMsg     term=none cterm=none ctermfg=38   ctermbg=none
  hi WildMenu       term=none cterm=none ctermfg=60   ctermbg=none
endif
