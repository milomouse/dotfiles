" Vim syntax file
" Language:	Gentoo package.use files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo package.use files. Requires vim 6.3 or
" later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-package-common.vim

syn match  GentooPackageUseUse contained
    \ /\([a-zA-Z0-9][a-zA-Z0-9\-_]*\|\*\)\(:\)\@!/
    \ nextgroup=GentooPackageUseUse,GentooPackageUseUnuse,
    \ GentooPackageUseExpand skipwhite
syn match  GentooPackageUseUnuse contained
    \ /-\([a-zA-Z0-9][a-zA-Z0-9\-_]*\|\*\)\(:\)\@!/
    \ nextgroup=GentooPackageUseUse,GentooPackageUseUnuse,
    \ GentooPackageUseExpand skipwhite
syn match  GentooPackageUseExpand contained
    \ /[a-zA-Z0-9][a-zA-Z0-9\-_]*:/
    \ nextgroup=GentooPackageUseUse,GentooPackageUseUnuse
    \ skipwhite
syn cluster GentooPackagePostAtom contains=GentooPackageUseUse,
    \ GentooPackageUseUnuse,GentooPackageUseExpand

hi def link GentooPackageUseUse              Special
hi def link GentooPackageUseUnuse            Keyword
hi def link GentooPackageUseExpand           Type

let b:current_syntax = "gentoo-package-use"
