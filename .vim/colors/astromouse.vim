" Vim color file
" Maintainer:   milomouse <vincent[at]fea.st>
" Last Change:  2010-02-06
" 256-- Colorful yet muted-- Console only-- Use at own discretion.

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="astromouse"

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
