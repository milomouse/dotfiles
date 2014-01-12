#! /bin/zsh
###########################################################
## locate: ${XDG_CONFIG_HOME}/herbstluftwm/dzen-post.zsh ##
## author: Vincent Z (github.com/milomouse)              ##
## detail: hlwm specific tag(s) information for `dzen2'  ##
###########################################################
## NOTE 1: relies on `herbstluftwm' to be up and running ##
## NOTE 2: script is dynamic and only updates on changes ##
## NOTE 3: able to be reloaded by `herbstclient reload'  ##
###########################################################

herbstclient getenv DISPLAY &>/dev/null || exit 1
source ${XDG_CONFIG_DIR:-$HOME}/herbstluftwm/dzen-colors.zsh
sep=("${b_bg}${c_08} Ϫ ${c_XX}${b_00}")

herbstclient --idle | while read i ; do
  if [[ $i == complete || $i =~ tag_ ]]; then
    tags=( $(herbstclient tag_status) ) || exit 2
    for i ( $tags ) {
      case ${i[1]} {
        '#') tags=("${tags/$i/${b_07}${c_00}${i#[[:graph:]]}${c_XX}${b_08} }") ;;
        '+') tags=("${tags/$i/${c_07}${i#[[:graph:]]}${c_XX} }") ;;
        '%') tags=("${tags/$i/${c_04}${i#[[:graph:]]}${c_XX} }") ;;
        '-') tags=("${tags/$i/${c_07}${i#[[:graph:]]}${c_XX} }") ;;
        '.') tags=("${tags/$i/${c_07}${i#[[:graph:]]}${c_XX} }") ;;
        ':')
          _n=(${#$(print ${(F)$(herbstclient layout ${i[2,-1]})} | grep '^0x')})
          case ${_n} {
            0) _N=' ' ;;
            1) _N='¹' ;;
            2) _N='²' ;;
            3) _N='³' ;;
            *) _N='֡' ;; #'keepforsyntax
          }
          tags=("${tags/$i/${c_05}${i#[[:graph:]]}${c_02}${_N}${c_XX}}")
        ;;
        '!') tags=("${tags/$i/${c_01}${i#[[:graph:]]}${c_XX} }") ;;
        *) tags=("${tags/$i/${c_07}${i#[[:graph:]]}${c_XX} }") ;;
      }
    }
    print "${b_00}${c_07} Ϡ ${c_fg}ɦerbstluftwm ${sep} ${tags:-${c_08}$(repeat 12 { printf " · " })}${_XX}"
  elif [[ $i =~ quit || $i =~ reload ]]; then
    kill $!
    exit
  fi
done | dzen2 -p -x 0 -y 0 -w 330 -h 16 -ta l -bg ${_bg} -fg ${_fg} \
       -fn '-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1' \
       &>/dev/null || exit 5
