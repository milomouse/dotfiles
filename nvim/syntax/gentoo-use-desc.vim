" Vim syntax file
" Language:	Gentoo use.desc, use.local.desc files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo package.keywords files. Requires vim 6.3 or
" later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-package-common.vim

syn match GentooUseDescPackage /^\(#\)\@!\([a-zA-Z0-9\-\_+\.]\+\/[a-zA-Z0-9\-\_+]\+\)\?/ nextgroup=GentooUseDescColon,GentooUseDescFlag skipwhite
syn match GentooUseDescColon /:/ contained nextgroup=GentooUseDescFlag
syn match GentooUseDescFlag contained /[a-zA-Z0-9\-\_+@:]\+/ nextgroup=GentooUseDescDash skipwhite
syn match GentooUseDescDash /-\s*/ contained nextgroup=GentooUseDescDesc skipwhite
syn region GentooUseDescDesc start=// end=/$/ contained skipwhite

hi def link GentooUseDescPackage          Keyword
hi def link GentooUseDescFlag             Identifier
hi def link GentooUseDescDesc             String

let b:current_syntax = "gentoo-package-keywords"
