" Vim filetype detection file
" Language:	Gentoo Things
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" This sets up syntax highlighting for Gentoo ebuilds, eclasses, GLEPs, init.d /
" conf.d / env.d / cron.d entries,  /etc/portage/ files and so on.
"

if &compatible || v:version < 603
    finish
endif


" ebuilds, eclasses
au BufNewFile,BufRead *.e{build,class}
    \     set filetype=ebuild

" GLEPs
au BufNewFile,BufRead *.txt,*.rst
    \ if (getline(1) =~? "^GLEP: " || getline(2) =~? "^GLEP: ") |
    \     set filetype=glep |
    \ endif

" /etc/init.d/ scripts
au BufNewFile,BufRead /etc/init.d/*
    \     set filetype=gentoo-init-d.sh |

au BufNewFile,BufRead *
    \ if (getline(1) =~? "#!/sbin/\\(runscript\\|openrc-run\\)") |
    \     set filetype=gentoo-init-d.sh |
    \ endif

" /etc/conf.d/ scripts
au BufNewFile,BufRead /etc/conf.d/*
    \     set filetype=gentoo-conf-d.sh

" /etc/env.d/ scripts
au BufNewFile,BufRead /etc/env.d/*
    \     set filetype=gentoo-env-d.sh

" /etc/cron.d/ scripts
au BufNewFile,BufRead /etc/cron.d/*
    \     set filetype=crontab

" package.mask, package.unmask
au BufNewFile,BufRead {*/package.{un,}mask,*/portage/package.{un,}mask/*}
    \     set filetype=gentoo-package-mask

" package.keywords
au BufNewFile,BufRead {*/package.{accept_,}keywords,*/portage/package.{accept_,}keywords/*}
    \     set filetype=gentoo-package-keywords

" package.use
au BufNewFile,BufRead {*/package.use,*/portage/package.use/*,*/package.env,*/portage/package.env/*}
    \     set filetype=gentoo-package-use

" package.license
au BufNewFile,BufRead {*/package.license,*/portage/package.license/*}
    \     set filetype=gentoo-package-license

" package.properties
au BufNewFile,BufRead {*/package.properties,*/portage/package.properties/*}
    \     set filetype=gentoo-package-properties

" thirdpartymirrors
au BufNewFile,BufRead {*/thirdpartymirrors,*/portage/mirrors}
    \     set filetype=gentoo-mirrors

" make.conf
au BufNewFile,BufRead {*/make.{conf,globals},*/portage/make.conf/*}
    \     set filetype=gentoo-make-conf

" use.desc
au BufNewFile,BufRead use.{local.,}desc
    \     set filetype=gentoo-use-desc

" metadata.xml
au BufNewFile,BufRead metadata.xml
    \     set filetype=gentoo-metadata

" repos.conf
au BufNewFile,BufRead {*/portage/repos.conf,*/portage/repos.conf/*.conf}
    \     set filetype=dosini

" portage/env/*
au BufNewFile,BufRead */portage/{env/*,bashrc}
    \     set filetype=ebuild

" guidexml
au BufNewFile,BufRead *.xml
    \     if getline(1) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(2) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(3) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(4) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(5) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(6) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(7) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(8) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(9) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " |
    \     set filetype=guidexml		      |
    \     endif
