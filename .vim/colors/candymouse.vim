" Vim color file
" Maintainers:  milomouse <vincent[at]fea.st>
" Last Change:  2009-10-20
" Inspiration:  "zenburn" and 'asu1dark"
" A muted yet slightly colorful vim style. Emphasis only on useful text.

set background=dark
hi clear          
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="candymouse"

if &t_Co > 255
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
    hi LineNr          ctermfg=141  ctermbg=235
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

    if exists("g:candymouse_high_Contrast")
      hi Normal ctermfg=188 ctermbg=234
    else
      hi Normal ctermfg=249 ctermbg=237
      hi Cursor        ctermbg=109
      hi diffadd       ctermbg=237
      hi diffdelete    ctermbg=238
      hi difftext      ctermbg=237
      hi errormsg      ctermbg=237
      hi foldcolumn    ctermbg=238
      hi folded        ctermbg=238
      hi incsearch     ctermbg=228
      hi linenr        ctermbg=238
      hi search        ctermbg=238
      hi statement     ctermbg=237
      hi statusline    ctermbg=60  ctermfg=13
      hi statuslinenc  ctermbg=5   ctermfg=7
      hi title         ctermbg=237
      hi todo          ctermbg=237
      hi underlined    ctermbg=237
      hi vertsplit     ctermbg=60
      hi visualnos     ctermbg=210
      hi warningmsg    ctermbg=236
      hi wildmenu      ctermbg=236
    endif
endif
