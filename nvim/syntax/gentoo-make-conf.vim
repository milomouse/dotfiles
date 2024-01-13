" Vim syntax file
" Language:	Gentoo make.conf Files
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for Gentoo make.conf files. Needs vim 6.3 or later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

runtime syntax/gentoo-common.vim

syn cluster GentooMakeConfEC add=GentooMakeConfEUse,GentooMakeConfEAK,GentooMakeConfEAL,GentooMakeConfEAP,GentooMakeConfEAT,GentooMakeConfECFLAGS,GentooMakeConfELDFLAGS,GentooMakeConfEMAKEOPTS,GentooMakeConfECHOST,GentooMakeConfEFEATURES,GentooMakeConfEMISC,GentooMakeConfEMISCK,GentooMakeConfEMISCKE,GentooMakeConfEMISCN
syn region  GentooMakeConfE start=/^/ end=/$/ contains=@GentooMakeConfEC,GentooMakeConfComment

" MISC {{{
syn match   GentooMakeConfEMISC /[a-zA-Z0-9\-\_]\+\([^a-zA-Z0-9\-\_]\)\@=/ contained nextgroup=GentooMakeConfEMISCE

syn match   GentooMakeConfEMISCE /=/ contained nextgroup=GentooMakeConfEMISCV,GentooMakeConfEMISCVNoQ skipwhite
syn region  GentooMakeConfEMISCV contained start=/"/ end=/"/ contains=GentooMakeConfEMISCIX
syn region  GentooMakeConfEMISCVNoQ contained start=/[^ "]/ end=/\s\|$/ contains=GentooMakeConfEMISCIX
syn match   GentooMakeConfEMISCIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

" naughty
syn match   GentooMakeConfEMISCN /LDFLAGS\|ASFLAGS\|ARCH\|ELIBC\|KERNEL\|USERLAND/ contained nextgroup=GentooMakeConfEMISCE
" known but not handled specially
syn match   GentooMakeConfEMISCK /GENTOO_MIRRORS\|SYNC\|PORTAGE_NICENESS\|PORTDIR_OVERLAY\|PORTAGE_GPG_DIR\|PORTAGE_GPG_KEY\|SIGNED_OFF_BY\|CONFIG_PROTECT_MASK\|CONFIG_PROTECT\|FETCHCOMMAND\|RESUMECOMMAND\|AUTOCLEAN\|BUILD_PREFIX\|CBUILD\|CLEAN_DELAY\|COLLISION_IGNORE\|DISTDIR\|DOC_SYMLINKS_DIR\|EMERGE_DEFAULT_OPTS\|HTTP_PROXY\|FTP_PROXY\|NOCOLOR\|PKGDIR\|PORT_LOGDIR\|PORT_LOGDIR_CLEAN\|PORTAGE_BINHOST\|PORTAGE_BINHOST_HEADER_URI\|PORTAGE_BINPKG_FORMAT\|PORTAGE_BINPKG_TAR_OPTS\|PORTAGE_COMPRESS\|PORTAGE_COMPRESS_FLAGS\|PORTAGE_ELOG_CLASSES\|PORTAGE_ELOG_COMMAND\|PORTAGE_ELOG_MAILFROM\|PORTAGE_ELOG_MAILURI\|PORTAGE_ELOG_SYSTEM\|PORTAGE_FETCH_CHECKSUM_TRY_MIRRORS\|PORTAGE_FETCH_RESUME_MIN_SIZE\|PORTAGE_LOGDIR\|PORTAGE_LOGDIR_CLEAN\|PORTAGE_RSYNC_EXTRA_OPTS\|PORTAGE_RSYNC_OPTS\|PORTAGE_RSYNC_INITIAL_TIMEOUT\|PORTAGE_RSYNC_RETRIES\|PORTAGE_TMPDIR\|PORTAGE_WORKDIR_MODE\|PORTDIR\|ROOT\|RSYNC_EXCLUDEFROM\|RSYNC_RETRIES\|RSYNC_TIMEOUT\|RPMDIR\|USE_ORDER\|LINGUAS\|EXTRA_ECONF\|PORTAGE_TMPFS\|INSTALL_MASK\|QA_STRICT_EXECSTACK\|QA_STRICT_WX_LOAD\|QA_STRICT_TEXTRELS\|PORTAGE_IONICE_COMMAND\|PORTAGE_LOG_FILTER_FILE_CMD\|PORTAGE_SCHEDULING_POLICY\|BINPKG_COMPRESS\|BINPKG_COMPRESS_FLAGS/ contained nextgroup=GentooMakeConfEMISCE
" common eclass stuff
syn match GentooMakeConfEMISCKE /EBEEP_IGNORE\|EPAUSE_IGNORE\|CHECKREQS_DONOTHING\|BREAKME\|ECHANGELOG_USER\|CCACHE_SIZE\|CCACHE_DIR\|CCACHE_SLOPPINESS\|DISTCC_DIR/ contained nextgroup=GentooMakeConfEMISCE

hi def link GentooMakeConfEMISC       Keyword
hi def link GentooMakeConfEMISCK      Identifier
hi def link GentooMakeConfEMISCN      Error
hi def link GentooMakeConfEMISCKE     Special
hi def link GentooMakeConfEMISCV      String
hi def link GentooMakeConfEMISCVNoQ   Constant
hi def link GentooMakeConfEMISCIB     Error
hi def link GentooMakeConfEMISCIX     Preproc
" }}}

" USE {{{
syn keyword GentooMakeConfEUse USE contained nextgroup=GentooMakeConfEUseE
syn match   GentooMakeConfEUseE /=/ contained nextgroup=GentooMakeConfEUseV skipwhite
syn cluster GentooMakeConfEUseIC add=GentooMakeConfEUseID,GentooMakeConfEUseIE,GentooMakeConfEUseIG,GentooMakeConfEUseIB,GentooMakeConfEUseIX
syn region  GentooMakeConfEUseV contained start=/"/ end=/"/ contains=@GentooMakeConfEUseIC
syn match   GentooMakeConfEUseIE /[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEUseID /-[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEUseIG /-\?@[a-zA-Z0-9\-\_]\+\|-\*/ contained
syn match   GentooMakeConfEUseIB /+@\?[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEUseIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEUse       Identifier
hi def link GentooMakeConfEUseV      String
hi def link GentooMakeConfEUseID     Keyword
hi def link GentooMakeConfEUseIE     Special
hi def link GentooMakeConfEUseIG     Preproc
hi def link GentooMakeConfEUseIB     Error
hi def link GentooMakeConfEUseIX     Preproc
" }}}

" ACCEPT_KEYWORDS {{{
syn match   GentooMakeConfEAK /ACCEPT_KEYWORDS/ contained nextgroup=GentooMakeConfEAKE
syn match   GentooMakeConfEAKE /=/ contained nextgroup=GentooMakeConfEAKV skipwhite
syn cluster GentooMakeConfEAKIC add=GentooMakeConfEAKIS,GentooMakeConfEAKIU,GentooMakeConfEAKIB,GentooMakeConfEAKIX
syn region  GentooMakeConfEAKV contained start=/"/ end=/"/ contains=@GentooMakeConfEAKIC
" do not change keyword order!
syn match   GentooMakeConfEAKIS /alpha\|amd64\|amd64-fbsd\|amd64-linux\|arm\|arm64\|arm64-linux\|arm-linux\|hppa\|ia64\|loong\|m68k\|m68k-mint\|mips\|ppc\|ppc64\|ppc64-linux\|ppc-aix\|ppc-macos\|riscv\|s390\|sh\|sparc\|sparc64-solaris\|sparc-solaris\|x64-cygwin\|x64-macos\|x64-solaris\|x86\|x86-cygwin\|x86-fbsd\|x86-linux\|x86-macos\|x86-solaris\|x86-winnt/ contained
syn match   GentooMakeConfEAKIU /\~\(alpha\|amd64\|amd64-fbsd\|amd64-linux\|arm\|arm64\|arm64-linux\|arm-linux\|hppa\|ia64\|loong\|m68k\|m68k-mint\|mips\|ppc\|ppc64\|ppc64-linux\|ppc-aix\|ppc-macos\|riscv\|s390\|sh\|sparc\|sparc64-solaris\|sparc-solaris\|x64-cygwin\|x64-macos\|x64-solaris\|x86\|x86-cygwin\|x86-fbsd\|x86-linux\|x86-macos\|x86-solaris\|x86-winnt\)/ contained
syn match   GentooMakeConfEAKIB /-[a-zA-Z0-9\-\_]\+/ contained
syn match   GentooMakeConfEAKIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEAK       Identifier
hi def link GentooMakeConfEAKV      String
hi def link GentooMakeConfEAKIS     Keyword
hi def link GentooMakeConfEAKIU     Special
hi def link GentooMakeConfEAKIB     Error
hi def link GentooMakeConfEAKIX     Preproc
" }}}

" ACCEPT_LICENSE {{{
syn match   GentooMakeConfEAL /ACCEPT_LICENSE/ contained nextgroup=GentooMakeConfEALE
syn match   GentooMakeConfEALE /=/ contained nextgroup=GentooMakeConfEALV skipwhite
syn cluster GentooMakeConfEALIC add=GentooMakeConfEALIP,GentooMakeConfEALIS,GentooMakeConfEALIN,GentooMakeConfEALIX
syn region  GentooMakeConfEALV contained start=/"/ end=/"/ contains=@GentooMakeConfEALIC
syn match   GentooMakeConfEALIP /\*\|[a-zA-Z0-9\-_.+]\+/ contained
syn match   GentooMakeConfEALIS /@[a-zA-Z0-9\-_.+]\+/ contained
syn match   GentooMakeConfEALIN /-\*\|-@\?[a-zA-Z0-9\-_.+]\+/ contained
syn match   GentooMakeConfEALIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEAL       Identifier
hi def link GentooMakeConfEALV      String
hi def link GentooMakeConfEALIP     Keyword
hi def link GentooMakeConfEALIS     Special
hi def link GentooMakeConfEALIN     Error
hi def link GentooMakeConfEALIX     Preproc
" }}}

" ACCEPT_PROPERTIES {{{
syn match   GentooMakeConfEAP /ACCEPT_PROPERTIES/ contained nextgroup=GentooMakeConfEAPE
syn match   GentooMakeConfEAPE /=/ contained nextgroup=GentooMakeConfEAPV skipwhite
syn cluster GentooMakeConfEAPIC add=GentooMakeConfEAPIP,GentooMakeConfEAPIN,GentooMakeConfEAPIX
syn region  GentooMakeConfEAPV contained start=/"/ end=/"/ contains=@GentooMakeConfEAPIC
syn match   GentooMakeConfEAPIP /\*\|[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEAPIN /-\*\|-[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEAPIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEAP       Identifier
hi def link GentooMakeConfEAPV      String
hi def link GentooMakeConfEAPIP     Keyword
hi def link GentooMakeConfEAPIN     Error
hi def link GentooMakeConfEAPIX     Preproc
" }}}

" ALLOW_TEST {{{
syn match   GentooMakeConfEAT /ALLOW_TEST/ contained nextgroup=GentooMakeConfEATE
syn match   GentooMakeConfEATE /=/ contained nextgroup=GentooMakeConfEATV skipwhite
syn cluster GentooMakeConfEATIC add=GentooMakeConfEATIP,GentooMakeConfEATIX
syn region  GentooMakeConfEATV contained start=/"/ end=/"/ contains=@GentooMakeConfEATIC
syn match   GentooMakeConfEATIP /\s*\(network\|all\)\s*/ contained
syn match   GentooMakeConfEATIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEAT       Identifier
hi def link GentooMakeConfEATV      String
hi def link GentooMakeConfEATIP     Keyword
hi def link GentooMakeConfEATIX     Preproc
" }}}

" C*FLAGS and F*FLAGS {{{
syn match   GentooMakeConfECFLAGS /C\(XX\)\?FLAGS/ contained nextgroup=GentooMakeConfECFLAGSE
syn match   GentooMakeConfECFLAGS /FC\?FLAGS/ contained nextgroup=GentooMakeConfECFLAGSE
syn match   GentooMakeConfECFLAGSE /=/ contained nextgroup=GentooMakeConfECFLAGSV,GentooMakeConfECFLAGSVNoQ skipwhite
syn cluster GentooMakeConfECFLAGSIC add=GentooMakeConfECFLAGSIB1,GentooMakeConfECFLAGSIB2,GentooMakeConfECFLAGSIB3,GentooMakeConfECFLAGSIX
syn region  GentooMakeConfECFLAGSV contained start=/"/ end=/"/ contains=@GentooMakeConfECFLAGSIC
syn match   GentooMakeConfECFLAGSIB1 /-ffast-math\|-freduce-all-givs\|-mfpmath=sse,387\|-DNDEBUG\|-s\([a-zA-Z0-9\-\_]\)\@!\|-Wno\S\+\|x86.\?64\|-mvis/ contained
syn match   GentooMakeConfECFLAGSIB2 /-[0o][123s]/ contained
syn match   GentooMakeConfECFLAGSIB3 /\%(-Os\|-fPIC\|-fpic\|-DPIC\)\%(\(=\%(k8\|opteron\|athlon64\|athlon-fx\).*\)\@<=\|\(.*=\%(k8\|opteron\|athlon64\|athlon-fx\)\)\@=\)/
syn match   GentooMakeConfECFLAGSIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained
syn region  GentooMakeConfECFLAGSVNoQ contained start=/[^ "]/ end=/\s\|$/ contains=GentooMakeConfECFLAGSIX

hi def link GentooMakeConfECFLAGS       Identifier
hi def link GentooMakeConfECFLAGSV      String
hi def link GentooMakeConfECFLAGSVNoQ   Constant
hi def link GentooMakeConfECFLAGSIB1    Error
hi def link GentooMakeConfECFLAGSIB2    Error
hi def link GentooMakeConfECFLAGSIB3    Error
hi def link GentooMakeConfECFLAGSIX     Preproc
" }}}

" LDFLAGS {{{
syn match   GentooMakeConfELDFLAGS /LDFLAGS/ contained nextgroup=GentooMakeConfELDFLAGSE
syn match   GentooMakeConfELDFLAGSE /=/ contained nextgroup=GentooMakeConfELDFLAGSV,GentooMakeConfELDFLAGSVNoQ skipwhite
syn cluster GentooMakeConfELDFLAGSIC add=GentooMakeConfELDFLAGSIB1,GentooMakeConfELDFLAGSIB2,GentooMakeConfELDFLAGSIB3,GentooMakeConfELDFLAGSIX
syn region  GentooMakeConfELDFLAGSV contained start=/"/ end=/"/ contains=@GentooMakeConfELDFLAGSIC
syn match   GentooMakeConfELDFLAGSIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained
syn region  GentooMakeConfELDFLAGSVNoQ contained start=/[^ "]/ end=/\s\|$/ contains=GentooMakeConfELDFLAGSIX

hi def link GentooMakeConfELDFLAGS       Identifier
hi def link GentooMakeConfELDFLAGSV      String
hi def link GentooMakeConfELDFLAGSVNoQ   Constant
hi def link GentooMakeConfELDFLAGSIX     Preproc
" }}}

" MAKEOPTS {{{
syn match   GentooMakeConfEMAKEOPTS /MAKEOPTS/ contained nextgroup=GentooMakeConfEMAKEOPTSE
syn match   GentooMakeConfEMAKEOPTSE /=/ contained nextgroup=GentooMakeConfEMAKEOPTSV skipwhite
syn cluster GentooMakeConfEMAKEOPTSIC add=GentooMakeConfEMAKEOPTSIB
syn region  GentooMakeConfEMAKEOPTSV contained start=/"/ end=/"/ contains=@GentooMakeConfEMAKEOPTSIC
syn match   GentooMakeConfEMAKEOPTSIB /-j \+[0-9]\+/ contained

hi def link GentooMakeConfEMAKEOPTS       Identifier
hi def link GentooMakeConfEMAKEOPTSV      String
hi def link GentooMakeConfEMAKEOPTSIB     Error
" }}}

" CHOST {{{
syn match   GentooMakeConfECHOST /CHOST/ contained nextgroup=GentooMakeConfECHOSTE
syn match   GentooMakeConfECHOSTE /=/ contained nextgroup=GentooMakeConfECHOSTV,GentooMakeConfECHOSTVNoQ skipwhite
syn cluster GentooMakeConfECHOSTIC add=GentooMakeConfECHOSTIB
syn region  GentooMakeConfECHOSTV contained start=/"/ end=/"/ contains=@GentooMakeConfECHOSTIC
syn match   GentooMakeConfECHOSTIB /sparc\(-unknown-linux-gnu\)\@![^ ]\+/ contained
syn region  GentooMakeConfECHOSTVNoQ contained start=/[^ "]/ end=/\s\|$/ contains=GentooMakeConfECFLAGSIX

hi def link GentooMakeConfECHOST       Identifier
hi def link GentooMakeConfECHOSTV      String
hi def link GentooMakeConfECHOSTVNoQ   String
hi def link GentooMakeConfECHOSTIB     Error
" }}}

" FEATURES {{{
syn keyword GentooMakeConfEFEATURES FEATURES contained nextgroup=GentooMakeConfEFEATURESE
syn match   GentooMakeConfEFEATURESE /=/ contained nextgroup=GentooMakeConfEFEATURESV skipwhite
syn cluster GentooMakeConfEFEATURESIC add=GentooMakeConfEFEATURESID,GentooMakeConfEFEATURESIE,GentooMakeConfEFEATURESIB,GentooMakeConfEFEATURESIX
syn region  GentooMakeConfEFEATURESV contained start=/"/ end=/"/ contains=@GentooMakeConfEFEATURESIC
syn match   GentooMakeConfEFEATURESIE /[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEFEATURESID /-[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEFEATURESIB /+[a-zA-Z0-9\-_]\+/ contained
syn match   GentooMakeConfEFEATURESIX /\\.\|\$\({[^}]\+}\|[a-zA-Z0-9\-\_]\+\)/ contained

hi def link GentooMakeConfEFEATURES       Identifier
hi def link GentooMakeConfEFEATURESV      String
hi def link GentooMakeConfEFEATURESID     Keyword
hi def link GentooMakeConfEFEATURESIE     Special
hi def link GentooMakeConfEFEATURESIG     Preproc
hi def link GentooMakeConfEFEATURESIB     Error
hi def link GentooMakeConfEFEATURESIX     Preproc
" }}}

syn region  GentooMakeConfComment start=/#/ end=/$/ contains=GentooBug

hi def link GentooMakeConfComment    Comment


let b:current_syntax = "gentoo-make-conf"

" vim: set foldmethod=marker : "
