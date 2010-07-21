" Vim color file
" Maintainers:  milomouse <vincent[at]fea.st>
" Last Change:  2010-04-09
" Inspiration:  "zenburn" and 'asu1dark"
" A colorful vim style. Emphasis only on useful text.

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

    if exists("g:candymouse_high_Contrast")
      hi Normal ctermfg=188 ctermbg=234
    else
      hi Normal ctermfg=249 ctermbg=236
      hi Cursor        ctermbg=109
      hi diffadd       ctermbg=237
      hi diffdelete    ctermbg=238
      hi difftext      ctermbg=237
      hi errormsg      ctermbg=237
      hi foldcolumn    ctermbg=238
      hi folded        ctermbg=238
      hi incsearch     ctermbg=228
      hi linenr        ctermbg=236
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
else
  hi Normal         term=none cterm=none ctermfg=DarkGray    ctermbg=Black
  hi Boolean        term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi Character      term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi Comment        term=none cterm=none ctermfg=DarkGray    ctermbg=Black
  hi Conditional    term=none cterm=none ctermfg=DarkBlue    ctermbg=none
  hi Constant       term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi Cursor         term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi CursorLine     term=none cterm=none                     ctermbg=Cyan
  hi Debug          term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi Define         term=none cterm=none ctermfg=Gray        ctermbg=none
  hi Delimiter      term=none cterm=none ctermfg=Gray        ctermbg=none
  hi DiffAdd        term=none cterm=none ctermfg=DarkCyan    ctermbg=DarkGray
  hi DiffChange     term=none cterm=none                     ctermbg=DarkGray
  hi DiffDelete     term=none cterm=none ctermfg=DarkGray    ctermbg=Black
  hi DiffText       term=none cterm=none ctermfg=Gray        ctermbg=Black
  hi Directory      term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi ErrorMsg       term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi Error          term=none cterm=none ctermfg=Magenta     ctermbg=Black
  hi Exception      term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi Float          term=none cterm=none ctermfg=DarkMagenta ctermbg=none
  hi FoldColumn     term=none cterm=none ctermfg=DarkCyan    ctermbg=Black
  hi Folded         term=none cterm=none ctermfg=DarkCyan    ctermbg=none
  hi Function       term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi Identifier     term=none cterm=none ctermfg=DarkBlue    ctermbg=none
  hi IncSearch      term=none cterm=none ctermfg=DarkBlue    ctermbg=none
  hi Keyword        term=none cterm=none ctermfg=DarkMagenta ctermbg=none
  hi Label          term=none cterm=none ctermfg=DarkMagenta ctermbg=none
  hi LineNr         term=none cterm=none ctermfg=DarkGray    ctermbg=Black
  hi Macro          term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi ModeMsg        term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi MoreMsg        term=none cterm=none ctermfg=DarkCyan    ctermbg=none
  hi NonText        term=none cterm=none ctermfg=DarkGray    ctermbg=Black
  hi Number         term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi Operator       term=none cterm=none ctermfg=Cyan        ctermbg=none
  hi PreCondit      term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi PreProc        term=none cterm=none ctermfg=Gray        ctermbg=Black
  hi Question       term=none cterm=none ctermfg=DarkCyan    ctermbg=none
  hi Repeat         term=none cterm=none ctermfg=Blue        ctermbg=none
  hi Search         term=none cterm=none ctermfg=DarkBlue    ctermbg=none
  hi SpecialChar    term=none cterm=none ctermfg=Blue        ctermbg=none
  hi SpecialComment term=none cterm=none ctermfg=DarkBlue    ctermbg=none
  hi Special        term=none cterm=none ctermfg=Blue        ctermbg=none
  hi SpecialKey     term=none cterm=none ctermfg=Blue        ctermbg=none
  hi Statement      term=none cterm=none ctermfg=DarkCyan    ctermbg=none
  hi StatusLine     term=none cterm=none ctermfg=DarkGray    ctermbg=DarkCyan
  hi StatusLineNC   term=none cterm=none ctermfg=DarkBlue    ctermbg=DarkGray
  hi StorageClass   term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi String         term=none cterm=none ctermfg=DarkRed     ctermbg=none
  hi Structure      term=none cterm=none ctermfg=DarkMagenta ctermbg=none
  hi Tag            term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi Title          term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi Todo           term=none cterm=none ctermfg=Red         ctermbg=none
  hi Typedef        term=none cterm=none ctermfg=DarkCyan    ctermbg=none
  hi Type           term=none cterm=none ctermfg=DarkGray    ctermbg=none
  hi Underlined     term=none cterm=none ctermfg=Gray        ctermbg=none
  hi VertSplit      term=none cterm=none ctermfg=DarkBlue    ctermbg=DarkGray
  hi VisualNOS      term=none cterm=none ctermfg=Magenta     ctermbg=none
  hi WarningMsg     term=none cterm=none ctermfg=DarkMagenta ctermbg=none
  hi WildMenu       term=none cterm=none ctermfg=DarkBlue    ctermbg=none
endif
