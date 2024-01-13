" Vim syntax file
" Language:	Gentoo Common Rules
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603
    finish
endif

syn match  GentooBug contained /\(\([gG]entoo \|[dD]ebian \|[sS]ource[Ff]orge \)\?[Bb]ug \(#\s*\)\?\|#\)\d\{1,\}/

" bad space
syn region  GentooError start=/^ / end=/$/
" trailing whitespace
syn match   GentooError /\s\+$/
" mixed tab and space indentation
syn match   GentooError /\s*\(\t \| \t\)\s*/

hi def link GentooBug                    Underlined
hi def link GentooError                  Error
