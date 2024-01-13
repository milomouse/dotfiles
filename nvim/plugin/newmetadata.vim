" Vim plugin
" Purpose:      Intelligently create content for metadata.xml
" Author:       Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:    Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603 || exists("g:loaded_newmetadata")
    finish
endif

let g:loaded_newmetadata=1

runtime! plugin/gentoo-common.vim

fun! <SID>MakeNewMetadata()
    let l:pastebackup = &paste
    set nopaste

    " {{{ variables
    let l:filename = expand("%:p")
    let l:dir = expand("%:p:h")

    if glob(l:dir . '/*/*.ebuild') =~ '\S'
        let l:category = substitute(l:filename, '^.*/\([a-z][a-z0-9]\+-[a-z]\+\)/metadata.xml$',
                    \ '\1', '')
        let l:iscatmetadata = 1
    else
        let l:category = substitute(l:filename,
                    \ "^.*/\\([^/]\\+\\)/[^/]\\+/metadata\\.xml", "\\1", "g")
        let l:package = substitute(l:filename,
                    \ "^.*/\\([^/]\\+\\)/metadata\\.xml", "\\1", "g")
        let l:iscatmetadata = 0
        let l:user = GentooGetUser()
        let l:email = matchstr(l:user, "\\(<\\)\\@<=[^>]\\+\\(>\\)\\@=")
        let l:name = matchstr(l:user, "^[^<]\\+\\( <\\)\\@=")
    endif
    " }}}

    " {{{ catmetadata
    if l:iscatmetadata
        " {{{ content
        0 put ='<?xml version=\"1.0\" encoding=\"UTF-8\"?>'
        put ='<!DOCTYPE catmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">'
        put ='<catmetadata>'
        put ='<longdescription lang=\"en\">'
        put ='</longdescription>'
        put ='</catmetadata>'
        exec "normal gg=G"
        " }}}
    " }}}
    else
    " {{{ pkgmetadata

        " {{{ project
        let l:project = ""
        if l:category ==# "app-vim"
            let l:project = "vim"
        elseif l:category ==# "dev-haskell"
            let l:project = "haskell"
        elseif l:category ==# "dev-perl"
            let l:project = "perl"
        elseif l:category ==# "dev-php"
            let l:project = "php-bugs"
        elseif l:category ==# "dev-python"
            let l:project = "python"
        elseif l:category ==# "dev-ruby"
            let l:project = "ruby"
        elseif l:category ==# "dev-tex"
            let l:project = "tex"
        elseif l:category ==# "dev-java"
            let l:project = "java"
        endif
        " }}}

        " {{{ content
        0 put ='<?xml version=\"1.0\" encoding=\"UTF-8\"?>'
        put ='<!DOCTYPE pkgmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">'
        put ='<pkgmetadata>'
        if l:project != ""
            if l:project == "python"
                put ='<!-- please remove python@ if tests do not work -->'
            endif
            put ='<maintainer type=\"project\">'
            put ='<email>' . l:project . '@gentoo.org</email>'
            put ='</maintainer>'
        endif
        put ='<maintainer type=\"person\">'
        put ='<email>' . l:email . '</email>'
        if l:name != ""
            put ='<name>' . l:name . '</name>'
        endif
        put ='</maintainer>'
        if l:category ==# "dev-python"
            put ='<stabilize-allarches/>'
            put ='<upstream>'
            put ='<remote-id type=\"pypi\">' . l:package . '</remote-id>'
            put ='</upstream>'
        endif
        put ='</pkgmetadata>'
        exec "normal gg=G"
        " }}}
    endif
    " }}}

    " Jump back to the first line
    0

    " Delete last/empty line
    $ delete

    if pastebackup == 0
        set nopaste
    endif
endfun

com! -nargs=0 NewMetadata call <SID>MakeNewMetadata()
augroup NewMetadata
    au!
    autocmd BufNewFile metadata.xml
        \ call <SID>MakeNewMetadata()
augroup END

" vim: set et foldmethod=marker : "
