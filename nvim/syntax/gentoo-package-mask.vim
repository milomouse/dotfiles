" Vim syntax file
" Language:	Gentoo package.mask and package.unmask files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo package.mask and package.unmask files.
" Requires vim 6.3 or later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-package-common.vim

let b:current_syntax = "gentoo-package-mask"
