" Vim syntax file
" Language:	Gentoo guidexml
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for guidexml.xml. Inherits from xml.vim.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime! syntax/xml.vim
unlet b:current_syntax

syn cluster xmlTagHook add=guidexmlElement
syn keyword guidexmlElement contained mainpage guide news title subtitle
syn keyword guidexmlElement contained poster author abstract summary license
syn keyword guidexmlElement contained glsaindex glsa-latest version date
syn keyword guidexmlElement contained chapter section body figure fig img
syn keyword guidexmlElement contained br note impo warn pre p table tcolumn
syn keyword guidexmlElement contained tr th ti ul ol li b brite c comment
syn keyword guidexmlElement contained e i path mail uri dl dt dd newsitem
syn keyword guidexmlElement contained sup sub keyword ident const stmt var
syn keyword guidexmlElement contained book part include sections subsection
syn keyword guidexmlElement contained keyval values key
syn keyword guidexmlElement contained metadoc members lead member categories
syn keyword guidexmlElement contained cat files file docs doc memberof fileid
syn keyword guidexmlElement contained bugs bug dynamic intro listing list catid
syn keyword guidexmlElement contained overview inserts insert docdate

syn cluster xmlAttribHook add=guidexmlAttr
syn match guidexmlAttr contained /author\|caption\|category\|test\|parent/
syn match guidexmlAttr contained /gentoo\|id\|lang\|by\|linkto\|link/
syn match guidexmlAttr contained /short\|src\|title\|colspan\|rowspan/
syn match guidexmlAttr contained /type\|disclaimer\|redirect\|width\|align/
syn match guidexmlAttr contained /mail\|fullname\|vpart\|vchap\|stopper\|arch/
syn match guidexmlAttr contained /href\|metadoc\|name/

hi def link guidexmlElement Keyword
hi def link guidexmlAttr    Keyword

let b:current_syntax = "guidexml"
