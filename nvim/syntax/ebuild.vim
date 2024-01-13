" Vim syntax file
" Language:	Gentoo Ebuilds/Eclasses
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Syntax highlighting for ebuilds and eclasses. Inherits from sh.vim and adds
" in Gentoo-specific highlights for certain keywords and functions. Requires
" vim 6.3 or later.
"

if &compatible || v:version < 603
    finish
endif

if exists("b:current_syntax")
    finish
endif

let is_bash=1
runtime! syntax/sh.vim
unlet b:current_syntax

runtime syntax/gentoo-common.vim

" function names can contain more characters than sh.vim allows. Override
" this. See Gentoo bug 72469.
syn match bkshFunction	"^\s*\<\h[0-9a-zA-Z_\-\.]*\>\s*()"	skipwhite skipnl contains=bkshFunctionParen

" Default keywords
syn keyword EbuildCoreKeyword use has_version best_version use_with use_enable
syn keyword EbuildCoreKeyword keepdir econf die einstall einfo ewarn eqawarn eerror diropts
syn keyword EbuildCoreKeyword dobin docinto dodoc doexe doheader doinfo doins
syn keyword EbuildCoreKeyword dolib dolib.a dolib.so doman dosbin dosym emake exeinto
syn keyword EbuildCoreKeyword exeopts fowners fperms insinto insopts into libopts newbin
syn keyword EbuildCoreKeyword newexe newheader newins newman newsbin has unpack into
syn keyword EbuildCoreKeyword doinitd doconfd doenvd domo dodir ebegin eend
syn keyword EbuildCoreKeyword newconfd newdoc newenvd newinitd newlib.a newlib.so
syn keyword EbuildCoreKeyword hasv usev usex elog eapply eapply_user
syn keyword EbuildCoreKeyword einstalldocs in_iuse get_libdir
syn keyword EbuildCoreKeyword dostrip ver_cut ver_rs ver_test

" Deprecated and banned functions
syn keyword EbuildDeprecatedKeyword check_KV dohard dohtml prepall prepalldocs
syn keyword EbuildDeprecatedKeyword prepallinfo prepallman prepallstrip dosed
syn keyword EbuildDeprecatedKeyword dojar hasq useq

" Sandbox
syn keyword EbuildCoreKeyword addread addwrite adddeny addpredict

" Recognised functions
syn keyword EbuildFunctions pkg_pretend pkg_nofetch pkg_setup src_unpack src_compile src_test src_install
syn keyword EbuildFunctions pkg_preinst pkg_postinst pkg_prerm pkg_postrm pkg_config
syn keyword EbuildFunctions pkg_info src_prepare src_configure

" Default functions
syn keyword EbuildFunctions default
syn keyword EbuildFunctions default_pkg_nofetch default_src_unpack default_src_prepare
syn keyword EbuildFunctions default_src_configure default_src_compile default_src_test
syn keyword EbuildFunctions default_src_install

" Inherit
syn keyword EbuildInherit inherit

" autotools
syn keyword EbuildAutoKeyword eautoreconf eaclocal _elibtoolize eautoconf eautoheader eautomake

" eutils
syn keyword EbuildEutilsKeyword draw_line epatch have_NPTL get_number_of_jobs
syn keyword EbuildEutilsKeyword emktemp edos2unix make_desktop_entry strip-linguas
syn keyword EbuildEutilsKeyword make_session_desktop domenu doicon newicon
syn keyword EbuildEutilsKeyword preserve_old_lib preserve_old_lib_notify epunt_cxx
syn keyword EbuildEutilsKeyword make_wrapper

" deprecated & banned eutils functions
syn keyword EbuildDeprecatedKeyword draw_line have_NPTL get_number_of_jobs check_license
syn keyword EbuildDeprecatedKeyword ebeep epause built_with_use

" flag-o-matic
syn keyword EbuildFlagoKeyword setup-allowed-flags filter-flags filter-lfs-flags append-lfs-flags
syn keyword EbuildFlagoKeyword append-flags replace-flags replace-cpu-flags is-flag filter-mfpmath
syn keyword EbuildFlagoKeyword strip-flags test-flag test_version_info strip-unsupported-flags get-flag
syn keyword EbuildFlagoKeyword replace-sparc64-flags append-ldflags filter-ldflags
syn keyword EbuildFlagoKeyword append-cflags append-cppflags append-cxxflags append-fflags
syn keyword EbuildFlagoKeyword is-flagq is-ldflagq is-ldflag test-flag-CC test-flag-CXX
syn keyword EbuildFlagoKeyword test-flag-F77 test-flag-FC test-flags-CC test-flags-CXX
syn keyword EbuildFlagoKeyword test-flags-F77 test-flags-FC test-flags append-libs
syn keyword EbuildFlagoKeyword raw-ldflags no-as-needed

" libtool
syn keyword EbuildLibtoolKeyword elibtoolize uclibctoolize darwintoolize

" fixheadtails
syn keyword EbuildFixHeadTailsKeyword ht_fix_file ht_fix_all

" fdo-mime
syn keyword EbuildFdoMimeKeyword fdo-mime_desktop_database_update fdo-mime_mime_database_update

" webapp
syn keyword EbuildWebappKeyword webapp_checkfileexists webapp_import_config webapp_strip_appdir
syn keyword EbuildWebappKeyword webapp_strip_d webapp_strip_cwd webapp_configfile webapp_hook_script
syn keyword EbuildWebappKeyword webapp_postinst_txt webapp_postupgrade_txt webapp_runbycgibin
syn keyword EbuildWebappKeyword webapp_serverowned webapp_server_configfile webapp_sqlscript
syn keyword EbuildWebappKeyword webapp_src_install webapp_pkg_postinst webapp_pkg_setup
syn keyword EbuildWebappKeyword webapp_getinstalltype webapp_src_preinst webapp_pkg_prerm

" versionator
syn keyword EbuildVersionatorKeyword get_all_version_components version_is_at_least
syn keyword EbuildVersionatorKeyword get_version_components get_major_version
syn keyword EbuildVersionatorKeyword get_version_component_range get_after_major_version
syn keyword EbuildVersionatorKeyword replace_version_separator replace_all_version_separators
syn keyword EbuildVersionatorKeyword delete_version_separator delete_all_version_separators

" cvs
syn keyword EbuildCVSKeyword cvs_fetch cvs_src_unpack

" bash-completion (removed)
syn keyword EbuildDeprecatedKeyword dobashcompletion bash-completion_pkg_postinst

" bash-completion-r1
syn keyword EbuildBashCompKeyword dobashcomp newbashcomp get_bashcompdir
syn keyword EbuildBashCompKeyword get_bashhelpersdir bashcomp_alias

" vim-plugin
syn keyword EbuildVimPluginKeyword vim-plugin_src_install vim-plugin_pkg_postinst vim-plugin_pkg_postrm
syn keyword EbuildVimPluginKeyword update_vim_afterscripts display_vim_plugin_help

" vim-doc
syn keyword EbuildVimDocKeyword update_vim_helptags

" multilib
syn keyword EbuildMultilibKeyword has_multilib_profile get_multilibdir get_libdir_override
syn keyword EbuildMultilibKeyword get_abi_var get_abi_CFLAGS get_abi_LDFLAGS get_abi_CHOST
syn keyword EbuildMultilibKeyword get_abi_FAKE_TARGETS get_abi_CDEFINE get_abi_LIBDIR get_install_abis
syn keyword EbuildMultilibKeyword get_all_abis get_all_libdirs is_final_abi number_abis get_ml_incdir
syn keyword EbuildMultilibKeyword prep_ml_includes create_ml_includes create_ml_includes-absolute
syn keyword EbuildMultilibKeyword create_ml_includes-tidy_path create_ml_includes-listdirs
syn keyword EbuildMultilibKeyword create_ml_includes-makedestdirs create_ml_includes-allfiles
syn keyword EbuildMultilibKeyword create_ml_includes-sym_for_dir

" toolchain-funcs
syn keyword EbuildToolFuncsKeyword tc-getPROG tc-getAR tc-getAS tc-getCC tc-getCXX tc-getLD tc-getNM
syn keyword EbuildToolFuncsKeyword tc-getRANLIB tc-getF77 tc-getGCJ tc-getBUILD_CC tc-export ninj
syn keyword EbuildToolFuncsKeyword tc-is-cross-compiler tc-ninja_magic_to_arch tc-arch-kernel tc-arch
syn keyword EbuildToolFuncsKeyword tc-endian gcc-fullversion gcc-version gcc-major-version
syn keyword EbuildToolFuncsKeyword gcc-minor-version gcc-micro-version gen_usr_ldscript

" cron
syn keyword EbuildCronKeyword docrondir docron docrontab cron_pkg_postinst

" games (deprecated)
syn keyword EbuildDeprecatedKeyword egamesconf egamesinstall gameswrapper dogamesbin dogamessbin dogameslib
syn keyword EbuildDeprecatedKeyword dogameslib.a dogameslib.so newgamesbin newgamessbin gamesowners gamesperms
syn keyword EbuildDeprecatedKeyword prepgamesdirs gamesenv games_pkg_setup games_src_compile games_pkg_postinst
syn keyword EbuildDeprecatedKeyword games_ut_unpack games_umod_unpack games_make_wrapper

" subversion
syn keyword EbuildSVNKeyword subversion_fetch subversion_bootstrap subversion_src_unpack

" alternatives
syn keyword EbuildAltKeyword alternatives_auto_makesym alternatives_makesym alternatives_pkg_postinst
syn keyword EbuildAltKeyword alternatives_pkg_postrm

" rpm
syn keyword EbuildRPMKeyword rpm_unpack rpm_src_unpack

" python (deprecated)
syn keyword EbuildDeprecatedKeyword python_pkg_setup python_convert_shebangs python_clean_installation_image
syn keyword EbuildDeprecatedKeyword python_src_prepare python_src_configure python_src_compile python_src_test
syn keyword EbuildDeprecatedKeyword python_src_install python_execute_function
syn keyword EbuildDeprecatedKeyword python_generate_wrapper_scripts python_set_active_version python_need_rebuild
syn keyword EbuildDeprecatedKeyword PYTHON python_get_implementation python_get_implementational_package
syn keyword EbuildDeprecatedKeyword python_get_libdir python_get_library
syn keyword EbuildDeprecatedKeyword python_get_version python_execute_nosetests python_execute_py.test
syn keyword EbuildDeprecatedKeyword python_execute_trial python_enable_pyc python_disable_pyc python_mod_optimize
syn keyword EbuildDeprecatedKeyword python_mod_cleanup

" python-utils-r1
syn keyword EbuildPythonKeyword python_export python_get_sitedir python_get_includedir
syn keyword EbuildPythonKeyword python_get_library_path python_get_CFLAGS python_get_LIBS
syn keyword EbuildPythonKeyword python_get_PYTHON_CONFIG python_get_scriptdir
syn keyword EbuildPythonKeyword python_optimize python_scriptinto python_doexe
syn keyword EbuildPythonKeyword python_newexe python_doscript python_newscript
syn keyword EbuildPythonKeyword python_moduleinto python_domodule python_doheader
syn keyword EbuildPythonKeyword python_wrapper_setup python_is_python3 python_is_installed
syn keyword EbuildPythonKeyword python_fix_shebang python_export_utf8_locale build_sphinx
syn keyword EbuildPythonKeyword epytest eunittest

" python-r1, python-single-r1 and python-any-r1
syn keyword EbuildPythonKeyword python_gen_usedep python_gen_useflags python_gen_cond_dep
syn keyword EbuildPythonKeyword python_gen_impl_dep python_copy_sources python_foreach_impl
syn keyword EbuildPythonKeyword python_setup python_replicate_script python_gen_any_dep
syn keyword EbuildPythonKeyword python-single-r1_pkg_setup python-any-r1_pkg_setup
syn keyword EbuildPythonKeyword python_check_deps

" deprecated functions
syn keyword EbuildDeprecatedKeyword python_parallel_foreach_impl python_export_best

" perl-module
syn keyword EbuildPerlModuleKeyword perl-module_src_prep perl-module_src_compile perl-module_src_test
syn keyword EbuildPerlModuleKeyword perl-module_src_install perl-module_pkg_setup perl-module_pkg_preinst
syn keyword EbuildPerlModuleKeyword perl-module_pkg_postinst perl-module_pkg_prerm perl-module_pkg_postrm
syn keyword EbuildPerlModuleKeyword perlinfo fixlocalpod updatepod

" distutils (deprecated)
syn keyword EbuildDeprecatedKeyword distutils_src_unpack distutils_src_prepare distutils_src_compile
syn keyword EbuildDeprecatedKeyword distutils_src_test distutils_src_install distutils_pkg_postinst
syn keyword EbuildDeprecatedKeyword distutils_pkg_postrm

" distutils-r1
syn keyword EbuildDistutilsKeyword distutils_install_for_testing
syn keyword EbuildDistutilsKeyword distutils-r1_python_prepare_all
syn keyword EbuildDistutilsKeyword distutils-r1_python_compile
syn keyword EbuildDistutilsKeyword distutils-r1_python_install
syn keyword EbuildDistutilsKeyword distutils-r1_python_install_all
syn keyword EbuildDistutilsKeyword sphinx_compile_all
syn match EbuildDistutilsKeyword "esetup\.py"

" distutils-r1 global helpers
syn keyword EbuildDistutilsKeyword distutils_enable_tests
syn keyword EbuildDistutilsKeyword distutils_enable_sphinx

" distutils-r1 sub-phases
syn keyword EbuildDistutilsFunction python_prepare python_prepare_all
syn keyword EbuildDistutilsFunction python_configure python_configure_all
syn keyword EbuildDistutilsFunction python_compile python_compile_all
syn keyword EbuildDistutilsFunction python_test python_test_all
syn keyword EbuildDistutilsFunction python_install python_install_all

" depend.apache
syn keyword EbuildDependApacheKeyword need_apache need_apache1 need_apache2

" apache-module
syn keyword EbuildApacheModuleKeyword apache-module_pkg_setup apache-module_src_compile
syn keyword EbuildApacheModuleKeyword apache-module_src_install apache-module_pkg_postinst acache_cd_dir
syn keyword EbuildApacheModuleKeyword apache_mod_file apache_doc_magic apache1_src_compile apache1_src_install
syn keyword EbuildApacheModuleKeyword apache1_pkg_postinst apache2_pkg_setup apache2_src_compile
syn keyword EbuildApacheModuleKeyword apache1_src_install apache2_pkg_postinst

" pam
syn keyword EbuildPamKeyword dopamd newpamd dopamsecurity newpamsecurity getpam_mod_dir
syn keyword EbuildPamKeyword dopammod newpammod pamd_mimic_system

" virtualx
syn keyword EbuildVirtualXKeyword virtualmake Xmake Xemake Xeconf

" gnome2
syn keyword EbuildGnome2Keyword gnome2_src_configure gnome2_src_compile gnome2_src_install
syn keyword EbuildGnome2Keyword gnome2_gconf_install gnome2_gconf_uninstal gnome2_omf_fix
syn keyword EbuildGnome2Keyword gnome2_scrollkeeper_update gnome2_pkg_postinst gnome2_pkg_postrm

" cdrom
syn keyword EbuildCDROMKeyword cdrom_get_cds cdrom_load_next

" linux-info
syn keyword EbuildLinuxInfoKeyword set_arch_to_kernel set_arch_to_portage

" unpacker
syn keyword EbuildUnpackerKeyword unpack_pdv unpack_makeself

" user
syn keyword EbuildDeprecatedKeyword enewuser enewgroup
syn keyword EbuildUserKeyword egetent

" cmake
syn keyword EbuildCMakeKeyword cmake_run_in cmake_comment_add_subdirectory cmake_use_find_package
syn keyword EbuildCMakeKeyword cmake_build mycmakeargs MYCMAKEARGS
syn keyword EbuildCMakeKeyword cmake_src_prepare cmake_src_configure cmake_src_compile
syn keyword EbuildCMakeKeyword cmake_src_test cmake_src_install

" tmpfiles
syn keyword EbuildTmpfilesKeyword dotmpfiles newtmpfiles tmpfiles_process

" udev
syn keyword EbuildUdevKeyword get_udevdir udev_dorules udev_newrules udev_reload
syn keyword EbuildDeprecatedKeyword udev_get_udevdir

" check-reqs
syn keyword EbuildCheckReqsKeyword check-reqs_pkg_setup check-reqs_pkg_pretend

" EXPORT_FUNCTIONS
syn match EbuildExportFunctions /EXPORT_FUNCTIONS/ skipwhite nextgroup=EbuildExportFunctionsFunc,EbuildExportFunctionsFuncE
syn match EbuildExportFunctionsFunc contained /\S\+\(\s\|$\)\@=/ skipwhite nextgroup=EbuildExportFunctionsFunc,EbuildExportFunctionsFuncE
syn match EbuildExportFunctionsFuncE contained /\S\+\(\s\|$\)\@=\(\${\S\+}\|pkg_pretend\|pkg_nofetch\|pkg_setup\|src_unpack\|src_prepare\|src_configure\|src_compile\|src_test\|src_install\|pkg_preinst\|pkg_postinst\|pkg_prerm\|pkg_postrm\|pkg_config\|pkg_info\)\@<!/ skipwhite nextgroup=EbuildExportFunctionsFunc,EbuildExportFunctionsFuncE

" Eclass documentation
syn match   EclassDocumentationTag /@\(DEAD\|DEFAULT_UNSET\|INCLUDES_EPREFIX\|INTERNAL\|OUTPUT_VARIABLE\|PRE_INHERIT\|REQUIRED\|USER_VARIABLE\)$/ contained
syn match   EclassDocumentationTagAndColon /@\(AUTHOR\|BLURB\|BUGREPORTS\|CODE\|DEPRECATED\|DESCRIPTION\|ECLASS_VARIABLE\|ECLASS\|EXAMPLE\|FUNCTION\|MAINTAINER\|PROVIDES\|RETURN\|SUBSECTION\|SUPPORTED_EAPIS\|USAGE\|VARIABLE\|VCSURL\):/ contained
syn cluster EclassDocumentation contains=EclassDocumentationTag,EclassDocumentationTagAndColon
" use shComment (sh.vim), make it compatible with other comment highlights
syn match      shComment        "^\s*\zs#.*$"   contains=@EclassDocumentation
syn match      shComment        "\s\zs#.*$"     contains=@EclassDocumentation

" mistakes: misspelling
syn keyword EbuildError LICENCE
" non-GLEP 23
syn match   EbuildError /LICENSE="[^|]*|[^|].*"/
" read only
syn match   EbuildError /^\(P\|PN\|PV\|PR\|PVR\|PF\|A\)=/
" default values
syn match   EbuildError ~^S="\?\${\?WORKDIR}\?/\${\?P}\?"\?\s*$~
" not allowed
syn match   EbuildError /SLOT\s*=\s*\(""\|''\|$\)/
" Don't be overly strict (Funtoo allows * and ~* as noarch ebuild keywords)
" syn match   EbuildError /KEYWORDS\s*=\s*.*[^-]\*.*/
" evil syntax, ask Mr_Bones_
syn match   EbuildError /^[a-zA-Z0-9\-\_]\+ ()/
syn match   EbuildError /^[a-zA-Z0-9\-\_]\+(){/
" should be epause
syn keyword EbuildError esleep
" should be ${P}
syn match   EbuildErrorC /\${PN}-\${PV}/

" trailing space
if exists("g:ebuild_error_on_trailing_whitespace")
	echohl WarningMsg | echo "g:ebuild_error_on_trailing_whitespace is deprecated, bad space highlight is enabled by default." | echohl None
endif

" prepalldocs is 'strongly discouraged'; decided by the Council
syn keyword EbuildError prepalldocs

" these can be contained within strings etc...
" not to be used in an ebuild
syn match   EbuildErrorC /\$\?{\?EXTRA_ECONF}\?/
" stupid cat usage
syn match   EbuildErrorC /\(z\)\@<!cat \S\+\s\+|\s*\(sed\|[aef]\?grep\|sort\|cut\|head\|tail\|patch\)/
" Use type -P instead of `which`
syn match   EbuildErrorC /`which.*`\|$(which.*)/

" Special homepage handling
syn match EbuildHomePageError /^HOMEPAGE=.*\(\${[^}]*}\?\|\([^\\]\)\@<=\$[^{]\w*\).*$/

" Too long descriptions
syn match   EbuildErrorC /^DESCRIPTION=['"].\{81,\}['"]$/

" clusters
syn cluster EbuildThings contains=EbuildCoreKeyword,EbuildFunctions,EbuildInherit,EbuildEutilsKeyword
syn cluster EbuildThings add=EbuildLibtoolKeyword,EbuildFixHeadTailsKeyword,EbuildWebappKeyword
syn cluster EbuildThings add=EbuildFlagoKeyword,EbuildError,EbuildVersionatorKeyword
syn cluster EbuildThings add=EbuildLibtoolKeyword,EbuildHomePageError,EbuildErrorC,EbuildCVSKeyword
syn cluster EbuildThings add=EbuildBashCompKeyword,EbuildVimPluginKeyword,EbuildVimDocKeyword
syn cluster EbuildThings add=EbuildFdoMimeKeyword,EbuildMultilibKeyword
syn cluster EbuildThings add=EbuildCronKeyword,EbuildToolFuncsKeyword
syn cluster EbuildThings add=EbuildSVNKeyword,EbuildAltKeyword,EbuildRPMKeyword,EbuildPythonKeyword
syn cluster EbuildThings add=EbuildPerlModuleKeyword,EbuildDistutilsKeyword
syn cluster EbuildThings add=EbuildDependApacheKeyword,EbuildApacheModuleKeyword,EbuildPamKeyword
syn cluster EbuildThings add=EbuildVirtualXKeyword,EbuildGnome2Keyword,EbuildAutoKeyword
syn cluster EbuildThings add=EbuildDeprecatedKeyword,EbuildUnpackerKeyword,EbuildUserKeyword
syn cluster EbuildThings add=EbuildCDROMKeyword,EbuildLinuxInfoKeyword,EbuildDistutilsFunction
syn cluster EbuildThings add=EbuildCMakeKeyword,EbuildCMakeFunction,EbuildTmpfilesKeyword
syn cluster EbuildThings add=EbuildUdevKeyword,EbuildCheckReqsKeyword

syn cluster shCommandSubList add=@EbuildThings
syn cluster shCommentGroup add=GentooBug
syn cluster shDblQuoteList add=EbuildErrorC,GentooError
syn cluster shExprList2 add=GentooError

hi def link EbuildCoreKeyword                Keyword
hi def link EbuildDeprecatedKeyword          Error
hi def link EbuildFunctions                  Special
hi def link EbuildInherit                    Include

hi def link EbuildEutilsKeyword              Identifier
hi def link EbuildFlagoKeyword               Identifier
hi def link EbuildLibtoolKeyword             Identifier
hi def link EbuildFixHeadTailsKeyword        Identifier
hi def link EbuildFdoMimeKeyword             Identifier
hi def link EbuildWebappKeyword              Identifier
hi def link EbuildVersionatorKeyword         Identifier
hi def link EbuildCVSKeyword                 Identifier
hi def link EbuildBashCompKeyword            Identifier
hi def link EbuildVimPluginKeyword           Identifier
hi def link EbuildVimDocKeyword              Identifier
hi def link EbuildMultilibKeyword            Identifier
hi def link EbuildCronKeyword                Identifier
hi def link EbuildToolFuncsKeyword           Identifier
hi def link EbuildSVNKeyword                 Identifier
hi def link EbuildAltKeyword                 Identifier
hi def link EbuildRPMKeyword                 Identifier
hi def link EbuildPythonKeyword              Identifier
hi def link EbuildPerlModuleKeyword          Identifier
hi def link EbuildDistutilsKeyword           Identifier
hi def link EbuildDependApacheKeyword        Identifier
hi def link EbuildApacheModuleKeyword        Identifier
hi def link EbuildPamKeyword                 Identifier
hi def link EbuildVirtualXKeyword            Identifier
hi def link EbuildGnome2Keyword              Identifier
hi def link EbuildAutoKeyword                Identifier
hi def link EbuildCDROMKeyword               Identifier
hi def link EbuildLinuxInfoKeyword           Identifier
hi def link EbuildUnpackerKeyword            Identifier
hi def link EbuildUserKeyword                Identifier
hi def link EbuildCMakeKeyword               Identifier
hi def link EbuildTmpfilesKeyword            Identifier
hi def link EbuildUdevKeyword                Identifier
hi def link EbuildCheckReqsKeyword           Identifier
hi def link EbuildDistutilsFunction          Special

hi def link EclassDocumentationTag           Identifier
hi def link EclassDocumentationTagAndColon   Identifier

hi def link EbuildHomePageError              Error
hi def link EbuildError                      Error
hi def link EbuildErrorC                     Error

hi def link EbuildExportFunctions            Constant
hi def link EbuildExportFunctionsFunc        Identifier
hi def link EbuildExportFunctionsFuncE       Error

let b:current_syntax = "ebuild"
