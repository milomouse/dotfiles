" Vim plugin
" Purpose:      New GLEP skeleton
" Author:       Michał Górny <mgorny@gentoo.org>
" Copyright:    Copyright (c) 2017 Michał Górny
" Licence:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603 || exists("g:loaded_newglep")
    finish
endif

let g:loaded_newglep=1

runtime! plugin/gentoo-common.vim

fun! <SID>MakeNewGLEP()
    let l:pastebackup = &paste
    set nopaste

    " {{{ variables
    let l:filename = expand("%:t")
    let l:basename = expand("%:t:r")
    let l:number = str2nr(substitute(l:basename, "^glep-", "", ""))
    let l:date = strftime("%Y-%m-%d")
    " }}}

    " {{{ header preamble
    put ='---'
    put ='GLEP: ' . l:number
    put ='Title: '
    put ='Author: ' . GentooGetUser()
    put ='Type: Standards Track'
    put ='Status: Draft'
    put ='Version: 1'
    put ='Created: ' . l:date
    put ='Last-Modified: ' . l:date
    put ='Post-History: '
    put ='Content-Type: text/x-rst'
    put ='Requires: '
    put ='Replaces: '
    put ='---'
    " }}}

    " {{{ warn if .txt suffix is used
    if l:filename =~# ".txt\$"
        put =''
        put ='.. Warning: .txt suffix is obsolete, use \"' . l:basename . '.rst\" instead'
    endif
    " }}}

    " {{{ skeleton
    let l:sections = ['Abstract', 'Motivation', 'Specification', 'Rationale',
        \ 'Backwards Compatibility', 'Reference Implementation', 'References',
        \ 'Copyright']
    for l:section in l:sections
        let l:sectlen = len(l:section)
        put =''
        put =l:section
        put =repeat('=', l:sectlen)
        put =''
        if l:section != 'Copyright'
            put =''
            put =''
        endif
    endfor
    " copyright
    put ='This work is licensed under the Creative Commons Attribution-ShareAlike 4.0'
    put ='International License.  To view a copy of this license, visit'
    put ='https://creativecommons.org/licenses/by-sa/4.0/.'
    " }}}

    " {{{ go to the first thing to edit
    0
    del
    /^Title:/
    normal $
    nohls
    " }}}

    if pastebackup == 0
        set nopaste
    endif
endfun

com! -nargs=0 NewGLEP call <SID>MakeNewGLEP() | set filetype=glep

if !exists("g:glep_create_on_empty")
    " Enable autogeneration of GLEPs by default
    let g:glep_create_on_empty = 1
endif

" check to see if v:progname is vimdiff to disable new GLEP creation
if v:progname =~ "vimdiff"
    let g:glep_create_on_empty = 0
endif

augroup NewGLEP
    au!
    autocmd BufNewFile glep-[0-9][0-9][0-9][0-9].{txt,rst}
                \ if g:glep_create_on_empty |
                \    call <SID>MakeNewGLEP() | set filetype=glep |
                \ endif
augroup END

" vim: set et foldmethod=marker : "
