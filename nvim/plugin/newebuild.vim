" Vim plugin
" Purpose:      Intelligently create content for new ebuild files
" Author:       Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:    Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603 || exists("g:loaded_newebuild")
    finish
endif

let g:loaded_newebuild=1

runtime! plugin/gentoo-common.vim

fun! <SID>MakeNewEbuild()
    let l:pastebackup = &paste
    set nopaste

    " {{{ variables
    let l:arch = GentooGetArch()
    let l:filename = expand("%:p")
    let l:category = substitute(l:filename,
                \ "^.*/\\([^/]\\+\\)/[^/]\\+/[^/]\\+\\.ebuild", "\\1", "g")
    let l:package = substitute(l:filename,
                \ "^.*/\\([^/]\\+\\)/[^/]\\+\\.ebuild", "\\1", "g")

    " use empty keywords for live ebuilds
    if l:filename =~# "-9999\\+.ebuild\$"
        let l:keywords = ""
    else
        let l:keywords = "~" . l:arch
    endif

    " }}}

    call GentooHeader()

    if expand("%:e") =~# "eclass\$"
        " {{{ eclass special setup
        let l:eclass=expand("%:t")
        put ='# @ECLASS: ' . l:eclass
        put ='# @MAINTAINER:'
        put ='# ' . GentooGetUser()
        put ='# @AUTHOR:'
        put ='# ' . GentooGetUser()
        put ='# @SUPPORTED_EAPIS: 8'
        put ='# @BLURB: '
        put ='# @DESCRIPTION:'
        put =''
        put ='case ${EAPI} in'
        put ='	8) ;;'
        put ='	*) die \"${ECLASS}: EAPI ${EAPI:-0} not supported\" ;;'
        put ='esac'
        put =''
        let l:eclass_ident = substitute(toupper(l:eclass), "[^A-Z0-9]", "_", "g")
        put ='if [[ ! ${_' . l:eclass_ident . '} ]]; then'
        put ='_' . l:eclass_ident . '=1'
        put =''
        put ='fi'
        " }}}

        " {{{ go to the first thing to edit
        0
        /^# @BLURB:/
        normal $
        nohls
        " }}}
    else
        put ='EAPI=8'
        put =''
        if l:category ==# "acct-group"
            " {{{ acct-group special setup
            put ='inherit acct-group'
            put =''
            put ='ACCT_GROUP_ID='
            " }}}
        elseif l:category ==# "acct-user"
            " {{{ acct-user special setup
            let l:username=substitute(expand("%:t:r"),
                    \ "\\(.*\\)-[0-9]*.*", "\\1", "")
            put ='inherit acct-user'
            put =''
            put ='DESCRIPTION=\"\"'
            put ='ACCT_USER_ID='
            put ='ACCT_USER_GROUPS=( ' . l:username . ' )'
            put =''
            put ='acct-user_add_deps'
            " }}}
        elseif l:category ==# "app-vim"
            " {{{ app-vim special setup
            put ='#VIM_PLUGIN_VIM_VERSION=\"7.0\"'
            put ='inherit vim-plugin'
            put =''
            put ='DESCRIPTION=\"vim plugin: \"'
            put ='HOMEPAGE=\"http://www.vim.org/scripts/script.php?script_id=\"'
            put ='LICENSE=\"\"'
            put ='KEYWORDS=\"' . l:keywords . '\"'
            put =''
            put ='VIM_PLUGIN_HELPFILES=\"\"'
            put ='VIM_PLUGIN_HELPTEXT=\"\"'
            put ='VIM_PLUGIN_HELPURI=\"\"'
            put ='VIM_PLUGIN_MESSAGES=\"\"'
            put =''
            " }}}
        elseif l:category ==# "www-apache" && expand("%:t") =~# "^mod_"
            " {{{ www-apache default setup (for module pkgs)
            put ='inherit apache-module'
            put =''
            put ='DESCRIPTION=\"\"'
            put ='HOMEPAGE=\"\"'
            put ='LICENSE=\"\"'
            put =''
            put ='KEYWORDS=\"' . l:keywords . '\"'
            put ='SLOT=\"0\"'
            put =''
            put ='# See apache-module.eclass for more information.'
            put ='APACHE2_MOD_CONF=\"XX_${PN}\"'
            put ='APACHE2_MOD_DEFINE=\"\"'
            put =''
            put ='need_apache2'
            " }}}
        elseif l:category ==# "dev-java"
            " {{{ dev-java generation-2 default java-pkg-simple ebuild
            put ='JAVA_PKG_IUSE=\"doc source test\"'
            put ='MAVEN_ID=\"\"'
            put ='MAVEN_PROVIDES=\"\"'
            put ='JAVA_TESTING_FRAMEWORKS=\"junit-4\"'
            put =''
            put ='inherit java-pkg-2 java-pkg-simple'
            put =''
            put ='DESCRIPTION=\"\"'
            put ='HOMEPAGE=\"\"'
            put ='SRC_URI=\"\"'
            put ='S=\"${WORKDIR}/${P}\"'
            put =''
            put ='LICENSE=\"\"'
            put ='SLOT=\"0\"'
            put ='KEYWORDS=\"' . l:keywords . '\"'
            put =''
            put ='CP_DEPEND=\"\"'
            put =''
            put ='DEPEND=\"${CP_DEPEND}'
	    put ='	>=virtual/jdk-1.8:*\"'
            put ='RDEPEND=\"${CP_DEPEND}'
	    put ='	>=virtual/jre-1.8:*\"'
        elseif l:category ==# "dev-perl" || l:category ==# "perl-core"
            " {{{ perl modules default setup
            put ='DIST_AUTHOR=\"\"'
            put ='inherit perl-module'
            put =''
            put ='DESCRIPTION=\"\"'
            put =''
            put ='#LICENSE=\"\|\| ( Artistic GPL-1+ )\"'
            put ='SLOT=\"0\"'
            put ='KEYWORDS=\"' . l:keywords . '\"'
            put =''
            put ='RDEPEND=\"\"'
            put ='DEPEND=\"${RDEPEND}\"'
            " }}}
        else
            " {{{ standard default setup
            " {{{ extra inherits for some categories
            if l:category ==# "dev-python"
                put ='DISTUTILS_USE_PEP517=hatchling'
                put ='PYTHON_COMPAT=( ' . GentooGetPythonTargets() . ' )'
                put =''
                put ='inherit distutils-r1 pypi'
                put =''
            endif
            " }}}

            put ='DESCRIPTION=\"\"'

            if l:category ==# "dev-python"
                put ='HOMEPAGE=\"'
                put ='	https://pypi.org/project/' . l:package . '/'
                put ='\"'
            else
                put ='HOMEPAGE=\"\"'
                put ='SRC_URI=\"\"'
            endif

            put =''
            put ='LICENSE=\"\"'
            put ='SLOT=\"0\"'
            put ='KEYWORDS=\"' . l:keywords . '\"'
            put =''

            " {{{ extra deps for some categories
            if l:category ==# "dev-python"
                put ='RDEPEND=\"'
                put ='\"'
                put ='BDEPEND=\"'
                put ='	test? ('
                put ='	)'
                put ='\"'
                put =''
                put ='distutils_enable_tests pytest'
            else
                put ='DEPEND=\"\"'
                put ='RDEPEND=\"${DEPEND}\"'
                put ='BDEPEND=\"\"'
            endif
            " }}}
        endif

        " {{{ go to the first thing to edit
        0
        /^\(ACCT_GROUP_ID\|DIST_AUTHOR\|DESCRIPTION\)=/
        exec "normal f=ll"
        nohls
        " }}}
    endif

    if pastebackup == 0
        set nopaste
    endif
endfun

com! -nargs=0 NewEbuild call <SID>MakeNewEbuild()

if !exists("g:ebuild_create_on_empty")
    " Enable autogeneration of ebuilds by default
    let g:ebuild_create_on_empty = 1
endif

" check to see if v:progname is vimdiff to disable new ebuild creation
if v:progname =~ "vimdiff"
    let g:ebuild_create_on_empty = 0
endif

augroup NewEbuild
    au!
    autocmd BufNewFile *.e{build,class}
                \ if g:ebuild_create_on_empty |
                \    call <SID>MakeNewEbuild() |
                \ endif
augroup END

" vim: set et foldmethod=marker : "
