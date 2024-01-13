" Vim syntax file
" Language:	Gentoo package.*/use.desc/etc. common rules
" Author:	Michał Górny <mgorny@gentoo.org>
" Copyright:	Copyright (c) 2018 Michał Górny
" Licence:	You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603
    finish
endif

runtime syntax/gentoo-common.vim

syn region GentooPackageComment start=/#/ end=/$/
    \ contains=GentooPackageEmail,GentooPackageDate,GentooBug

syn match  GentooPackageEmail contained /<[^<>@[:space:]]\+@[^<>@.[:space:]]\+\.[^<>@[:space:]]\+>/
syn match  GentooPackageDate  contained /(\(\d\d\?\s\w\+\|\w\+\s\d\d\?\)\s\d\{4\})/

syn match  GentooPackageAtom /^[\ \t]*[^ \t\n#]\+\S*\/\S\+/
    \ nextgroup=@GentooPackagePostAtom skipwhite

hi def link GentooPackageComment          Comment
hi def link GentooPackageEmail            Special
hi def link GentooPackageDate             Number
hi def link GentooPackageAtom             Identifier
