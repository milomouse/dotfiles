" Vim syntax file
" Language:	Gentoo package.license files
" Author:	Dror Levin <spatz@gentoo.org
" Copyright:	Copyright (c) 2010 Dror Levin
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo package.license files. Requires vim 6.3 or
" later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-package-common.vim

syn match  GentooPackageLicenseLicense contained /\([a-zA-Z0-9\-_.+]\+\|\*\)/
    \ nextgroup=@GentooPackagePostAtom skipwhite
syn match  GentooPackageLicenseUnLicense contained /-\([a-zA-Z0-9\-_.+]\+\|\*\)/
    \ nextgroup=@GentooPackagePostAtom skipwhite
syn match  GentooPackageLicenseLicenseGroup contained /-\?@[a-zA-Z0-9\-_.+]\+/
    \ nextgroup=@GentooPackagePostAtom skipwhite
syn cluster GentooPackagePostAtom contains=GentooPackageLicenseLicense,
    \ GentooPackageLicenseUnLicense,GentooPackageLicenseLicenseGroup

hi def link GentooPackageLicenseLicense          Special
hi def link GentooPackageLicenseUnLicense        Keyword
hi def link GentooPackageLicenseLicenseGroup     Type

let b:current_syntax = "gentoo-package-license"
