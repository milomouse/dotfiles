" Vim plugin
" Purpose:      Common functionality for gentoo-syntax plugins
" Author:       Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:    Copyright (c) 2005 Ciaran McCreesh
" Licence:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603 || exists("g:loaded_gentoo_common")
    finish
endif

let g:loaded_gentoo_common=1

fun! GentooGetUser()
    let l:result = expand("\$ECHANGELOG_USER")
    if l:result ==# "\$ECHANGELOG_USER"
        let l:gitcfg = "git config "
        if executable("git")
            let l:email = trim(system(l:gitcfg . "user.email"))
            let l:name = trim(system(l:gitcfg . "user.name"))
        else
            let l:email = expand("$HOST")
            let l:name = expand("$USER")
        endif
        let l:result = l:name . " <" . l:email . ">"
    endif
    return l:result
endfun

fun! GentooHeader(...)
    " The shebang arg is optional
    let l:year = strftime("%Y")
    let l:copyright = '# Copyright ' . l:year . ' Gentoo Authors'

    " Only one arg allowed (shebang only)
    if a:0 == 1
        0 put =a:1 " Insert shebang
        put =l:copyright
    else
        0 put =l:copyright
    endif
    put ='# Distributed under the terms of the GNU General Public License v2'
    $
endfun

fun! GentooGetArch()
    if exists("g:gentooarch") && g:gentooarch != ""
        return g:gentooarch
    else
        let l:a = system("portageq envvar ACCEPT_KEYWORDS 2>/dev/null")
        let l:a = substitute(l:a, "[\\n~]", " ", "g")
        let l:a = substitute(l:a, "^\\s\\+", "", "")
        let l:a = substitute(l:a, "\\s.*", "", "")
        if l:a == ""
            let l:a = "amd64"
        endif
        let g:gentooarch = l:a
        return g:gentooarch
    endif
endfun

fun! GentooGetPythonTargets()
    if exists("g:gentoopythontargets") && g:gentoopythontargets != ""
        return g:gentoopythontargets
    else
        let l:pyexec_path = "/etc/python-exec/python-exec.conf"

        if filereadable(l:pyexec_path)
            let l:pys = readfile(l:pyexec_path)->filter("v:val =~ '^[^#-]'")
                                             \ ->sort()
            let l:impls = []
            let l:py3s = []
            for l:py in l:pys
                let l:m = l:py->matchstr("^python3.*")->matchstr("\\d*$")
                if !empty(l:m)
                    eval l:py3s->add(l:m)
                else
                    eval l:impls->add(l:py)
                endif
            endfor
            if len(l:py3s) ==# 1
                eval l:impls->add("python3_".l:py3s->join())
            elseif len(l:py3s) > 1
                let l:min = ""
                let l:max = ""
                eval l:py3s->sort('N')
                for l:py in l:py3s
                    if l:min ==# ""
                        let l:min = l:py
                        let l:max = l:py
                    elseif l:py ==# l:max + 1
                        let l:max = l:py
                    else
                        let l:max = ""
                        break
                    endif
                endfor

                if l:max !=# ""
                    eval l:impls->add("python3_{".l:min."..".l:max."}")
                else
                    eval l:impls->add("python3_{".l:py3s->join(",")."}")
                endif
            endif
            let l:py3 = flatten(l:impls)->join()
        endif
        if empty(l:py3)
            let l:py3 =
                \ system("python -c 'import epython; print(epython.EPYTHON)'")
                \ ->substitute("\n", "", "g")->substitute("[.]", "_", "g")
        endif

        let g:gentoopythontargets = l:py3
        return g:gentoopythontargets
    endif
endfun

" vim: set et foldmethod=marker sw=4 ts=4 : "
