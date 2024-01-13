" Vim plugin
" Purpose:	create content for new init.d scripts
" Author:	Aaron Walker <ka0ttic@gentoo.org>
" Copyright:	Copyright (c) 2005 Aaron Walker
" License:	You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603 || exists("g:loaded_newinitd")
    finish
endif

let g:loaded_newinitd=1

runtime! plugin/gentoo-common.vim

fun! <SID>MakeNewInitd()
    call GentooHeader('#!/sbin/openrc-run')

    " {{{ variables
    let l:scriptname = expand("%:t:r")
    " }}}
    "
    " {{{ common metadata
    put ='name=\"' . l:scriptname . ' daemon\"'
    put ='description=\"\"'
    put ='command=/usr/bin/' . l:scriptname
    put ='command_args=\"${' . l:scriptname . '_args}\"'
    " }}}

    " {{{ functions
    put =''
    put ='depend() {'
    put =''
    put ='}'
    " }}}

    " Jump back to the first line
    0
endfun

com! -nargs=0 NewInitd call <SID>MakeNewInitd() | set filetype=gentoo-init-d.sh

augroup NewInitd
    au!
    autocmd BufNewFile {/*/files/*.{rc*,init*},/etc/init.d/*}
        \ call <SID>MakeNewInitd() | set filetype=gentoo-init-d.sh
augroup END

" vim: set et foldmethod=marker : "
