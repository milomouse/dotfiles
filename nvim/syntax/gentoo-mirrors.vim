" Vim syntax file
" Language:	Gentoo thirdpartymirrors files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo thirdpartymirrors files. Requires vim 6.3 or
" later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-package-common.vim

syn match  GentooMirrorsAtom /^[^# \t]\+/
    \ nextgroup=GentooMirrorsHttpUrl,GentooMirrorsHttpsUrl,GentooMirrorsFtpUrl skipwhite
syn region GentooMirrorsHttpUrl contained start=/http:\/\// end=/\(\s\)\@=\|$/
    \ nextgroup=GentooMirrorsHttpUrl,GentooMirrorsHttpsUrl,GentooMirrorsFtpUrl skipwhite
syn region GentooMirrorsHttpsUrl contained start=/https:\/\// end=/\(\s\)\@=\|$/
    \ nextgroup=GentooMirrorsHttpUrl,GentooMirrorsHttpsUrl,GentooMirrorsFtpUrl skipwhite
syn region GentooMirrorsFtpUrl contained start=/ftp:\/\// end=/\(\s\)\@=\|$/
    \ nextgroup=GentooMirrorsHttpUrl,GentooMirrorsHttpsUrl,GentooMirrorsFtpUrl skipwhite

hi def link GentooMirrorsAtom             Identifier
hi def link GentooMirrorsHttpUrl          String
hi def link GentooMirrorsHttpsUrl         Keyword
hi def link GentooMirrorsFtpUrl           Special

let b:current_syntax = "gentoo-mirrors"
