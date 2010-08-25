" Vim color file
" Maintainer:   milomouse <vincent[at]fea.st>
" Last Change:  2010-08-18
" Colorful yet muted-- Made for 256-color console.

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="cottonmouse"

if &t_Co > 255
  hi Normal         term=none cterm=none ctermfg=59  ctermbg=233
  hi Boolean        term=none cterm=none ctermfg=9   ctermbg=none
  hi Character      term=none cterm=none ctermfg=9   ctermbg=none
  hi Comment        term=none cterm=none ctermfg=238 ctermbg=234
  hi Conditional    term=none cterm=none ctermfg=6   ctermbg=none
  hi Constant       term=none cterm=none ctermfg=13  ctermbg=none
  hi Cursor         term=none cterm=none ctermfg=233 ctermbg=109
  hi CursorLine     term=none cterm=none ctermfg=none ctermbg=8
  hi CursorColumn   term=none cterm=none ctermfg=none ctermbg=8
  hi Debug          term=none cterm=none ctermfg=181 ctermbg=none
  hi Define         term=none cterm=none ctermfg=13  ctermbg=none
  hi Delimiter      term=none cterm=none ctermfg=109 ctermbg=none
  hi DiffAdd        term=none cterm=none ctermfg=66  ctermbg=237
  hi DiffChange     term=none cterm=none             ctermbg=236
  hi DiffDelete     term=none cterm=none ctermfg=236 ctermbg=238
  hi DiffText       term=none cterm=none ctermfg=217 ctermbg=237
  hi Directory      term=none cterm=none ctermfg=188 ctermbg=none
  hi ErrorMsg       term=none cterm=none ctermfg=13  ctermbg=none
  hi Error          term=none cterm=none ctermfg=13  ctermbg=234
  hi Exception      term=none cterm=none ctermfg=249 ctermbg=none
  hi Float          term=none cterm=none ctermfg=251 ctermbg=none
  hi FoldColumn     term=none cterm=none ctermfg=97  ctermbg=238
  hi Folded         term=none cterm=none ctermfg=238 ctermbg=234
  hi Function       term=none cterm=none ctermfg=95  ctermbg=none
  hi Identifier     term=none cterm=none ctermfg=110 ctermbg=none
  hi IncSearch      term=none cterm=none ctermfg=131 ctermbg=none
  hi Keyword        term=none cterm=none ctermfg=140 ctermbg=none
  hi Label          term=none cterm=none ctermfg=187 ctermbg=none
  hi LineNr         term=none cterm=none ctermfg=235 ctermbg=233
  hi Macro          term=none cterm=none ctermfg=13  ctermbg=none
  hi ModeMsg        term=none cterm=none ctermfg=13  ctermbg=none
  hi MoreMsg        term=none cterm=none ctermfg=15  ctermbg=none
  hi NonText        term=none cterm=none ctermfg=238 ctermbg=234
  hi Number         term=none cterm=none ctermfg=141 ctermbg=none
  hi Operator       term=none cterm=none ctermfg=38  ctermbg=none
  hi PreCondit      term=none cterm=none ctermfg=180 ctermbg=none
  hi PreProc        term=none cterm=none ctermfg=243 ctermbg=none
  hi Question       term=none cterm=none ctermfg=15  ctermbg=none
  hi Repeat         term=none cterm=none ctermfg=131 ctermbg=none
  hi Search         term=none cterm=none ctermfg=9   ctermbg=none
  hi SpecialChar    term=none cterm=none ctermfg=181 ctermbg=none
  hi SpecialComment term=none cterm=none ctermfg=108 ctermbg=none
  hi Special        term=none cterm=none ctermfg=140 ctermbg=none
  hi SpecialKey     term=none cterm=none ctermfg=151 ctermbg=none
  hi Statement      term=none cterm=none ctermfg=103 ctermbg=none
  hi StatusLine     term=none cterm=none ctermfg=6   ctermbg=234
  hi StatusLineNC   term=none cterm=none ctermfg=234 ctermbg=59
  hi StorageClass   term=none cterm=none ctermfg=249 ctermbg=none
  hi String         term=none cterm=none ctermfg=7   ctermbg=none
  hi Structure      term=none cterm=none ctermfg=229 ctermbg=none
  hi Tag            term=none cterm=none ctermfg=10  ctermbg=none
  hi Title          term=none cterm=none ctermfg=7   ctermbg=none
  hi Todo           term=none cterm=none ctermfg=95  ctermbg=none
  hi Typedef        term=none cterm=none ctermfg=253 ctermbg=none
  hi Type           term=none cterm=none ctermfg=102 ctermbg=none
  hi Underlined     term=none cterm=none ctermfg=188 ctermbg=none
  hi VertSplit      term=none cterm=none ctermfg=236 ctermbg=59
  hi VisualNOS      term=none cterm=none ctermfg=5   ctermbg=none
  hi WarningMsg     term=none cterm=none ctermfg=15  ctermbg=none
  hi WildMenu       term=none cterm=none ctermfg=194 ctermbg=none
else " for linux console
  hi Normal 	       ctermfg=7   ctermbg=234
  hi Boolean         ctermfg=9  
  hi Character       ctermfg=9   
  hi Comment         ctermfg=5   
  hi Conditional     ctermfg=6   
  hi Constant        ctermfg=13   
  hi Cursor          ctermfg=233 ctermbg=109
  hi Debug           ctermfg=181
  hi Define          ctermfg=13
  hi Delimiter       ctermfg=109
  hi DiffAdd         ctermfg=66  ctermbg=237
  hi DiffChange      ctermbg=236
  hi DiffDelete      ctermfg=236 ctermbg=238
  hi DiffText        ctermfg=217 ctermbg=237
  hi Directory       ctermfg=188
  hi ErrorMsg        ctermfg=13  ctermbg=236
  hi Exception       ctermfg=249
  hi Float           ctermfg=251
  hi FoldColumn      ctermfg=109 ctermbg=238
  hi Folded          ctermfg=109 ctermbg=238
  hi Function        ctermfg=186
  hi Identifier      ctermfg=147
  hi IncSearch       ctermbg=228 ctermfg=238
  hi Keyword         ctermfg=66
  hi Label           ctermfg=187
  hi LineNr          ctermfg=141 ctermbg=235
  hi Macro           ctermfg=13
  hi ModeMsg         ctermfg=13  cterm=none
  hi MoreMsg         ctermfg=15
  hi NonText         ctermfg=238
  hi Number          ctermfg=219
  hi Operator        ctermfg=80
  hi PreCondit       ctermfg=180
  hi PreProc         ctermfg=2
  hi Question        ctermfg=15
  hi Repeat          ctermfg=245
  hi Search          ctermfg=9   ctermbg=236
  hi SpecialChar     ctermfg=181
  hi SpecialComment  ctermfg=108
  hi Special         ctermfg=9
  hi SpecialKey      ctermfg=151
  hi Statement       ctermfg=9   cterm=none
  hi StatusLine      ctermfg=0   ctermbg=186
  hi StatusLineNC    ctermfg=7   ctermbg=108
  hi StorageClass    ctermfg=249
  hi String          ctermfg=9
  hi Structure       ctermfg=229
  hi Tag             ctermfg=10
  hi Title           ctermfg=7   ctermbg=234
  hi Todo            ctermfg=95  ctermbg=234
  hi Typedef         ctermfg=253
  hi Type            ctermfg=187
  hi Underlined      ctermfg=188 ctermbg=234
  hi VertSplit       ctermfg=236 ctermbg=65
  hi VisualNOS       ctermfg=5   ctermbg=210
  hi WarningMsg      ctermfg=15  ctermbg=236
  hi WildMenu        ctermbg=236 ctermfg=194
  hi CursorLine      ctermbg=3   cterm=none
endif
