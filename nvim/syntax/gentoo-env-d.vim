" Vim syntax file
" Language:	Gentoo /etc/env.d/ files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo /etc/env.d/ files. Inherits from sh.vim
" and adds in Gentoo-specific highlights for certain keywords and functions.
" Requires vim 6.3 or later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

let is_bash=1
runtime! syntax/sh.vim
unlet b:current_syntax

runtime syntax/gentoo-common.vim
syn cluster shCommentGroup add=GentooBug

let b:current_syntax = "gentoo-env-d"
