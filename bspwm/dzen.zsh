#! /bin/zsh
###############################################
## locate: ${XDG_CONFIG_HOME}/bspm/dzen.zsh  ##
## author: milomouse (github.com/milomouse)  ##
## detail: dzen2 statusbar for `bspwm'       ##
###############################################
## NOTE 1: spawned and killed from "xinitrc" ##
## NOTE 2: uses Xresources for dzen2 colors  ##
###############################################


##+ OUTPUT VARIABLES:
i_xresources="${XDG_CONFIG_HOME:-/howl/conf}/xorg/Xresources"
o_dzen=$(whence -p dzen2)
o_name="bspwm"
o_height='15'
o_width='1592'
o_x='4'
o_y='2'
o_font='-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1'
#o_font='-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1'
c_XX='^fg()'
if [[ -s ${i_xresources} ]]; then
  <${i_xresources} | grep "^*" | while read c ; do
  case "${${(s. .)c:l}[1]}" {
#    '*background:') o_bg=${${(s. .)c}[-1]} ; c_bg=^fg(${${(s. .)c}[-1]}) ;;
    '*background:') o_bg='#222222' ; c_bg=^fg(${o_bg}) ;;
    '*foreground:') o_fg=${${(s. .)c}[-1]} ; c_fg=^fg(${${(s. .)c}[-1]}) ;;
    '*color0:') c_00=^fg(${${(s. .)c}[-1]}) ;;  ## black
    '*color8:') c_08=^fg(${${(s. .)c}[-1]}) ;;  ## black,bold
    '*color1:') c_01=^fg(${${(s. .)c}[-1]}) ;;  ## red
    '*color9:') c_09=^fg(${${(s. .)c}[-1]}) ;;  ## red,bold
    '*color2:') c_02=^fg(${${(s. .)c}[-1]}) ;;  ## green
    '*color10:') c_10=^fg(${${(s. .)c}[-1]}) ;; ## green,bold
    '*color3:') c_03=^fg(${${(s. .)c}[-1]}) ;;  ## yellow
    '*color11:') c_11=^fg(${${(s. .)c}[-1]}) ;; ## yellow,bold
    '*color4:') c_04=^fg(${${(s. .)c}[-1]}) ;;  ## blue
    '*color12:') c_12=^fg(${${(s. .)c}[-1]}) ;; ## blue,bold
    '*color5:') c_05=^fg(${${(s. .)c}[-1]}) ;;  ## magenta
    '*color13:') c_13=^fg(${${(s. .)c}[-1]}) ;; ## magenta,bold
    '*color6:') c_06=^fg(${${(s. .)c}[-1]}) ;;  ## cyan
    '*color14:') c_14=^fg(${${(s. .)c}[-1]}) ;; ## cyan,bold
    '*color7:') c_07=^fg(${${(s. .)c}[-1]}) ;;  ## white
    '*color15:') c_15=^fg(${${(s. .)c}[-1]}) ;; ## white,bold
  }
  done
fi

##+ INPUT FUNCTIONS:
function i_newmail {
  INBOX=$(print - ${(Fw)#$(find /howl/mail/*/INBOX/new -type f)})
  ALL=$(print - ${(Fw)#$(find /howl/mail/*/*/new -type f)})
  print - "\(${c_04}open ${c_07}:unread ${c_06}\'${c_05}${INBOX}${c_07}:${c_13}${ALL}${c_XX})"
}
function i_mifo {
  m_a=$(mifo -a "%D:2: _MIFO_ %B") ; [[ ${#m_a} -eq 0 ]] && m_a="/"
  m_A=${${m_a/ _MIFO_*}//_/ } ; [[ ${#m_A} -eq 0 ]] && m_A='<unknown>'
  [[ ${#${m_a/*_MIFO_ }} -gt 50 ]] && m_N="${${${m_a/*_MIFO_ }//_/ }[1,50]}.." || m_N="${${m_a/*_MIFO_ }//_/ }"
  [[ ${#${m_A}} -gt 50 ]] && m_A="${${m_A}[1,50]}.."
  printf "$(mifo -a ${c_XX}\(${c_12}#${c_06}\'${c_07}\(${c_05}load ${c_15}\(${c_13}mplayer ${c_07}:dir ${c_XX}\"${c_12}${m_A}${c_XX}\" \
${c_07}:name ${c_XX}\"${c_09}${m_N:-%B}${c_XX}\" ${c_07}:type ${c_XX}\"${c_fg}%e${c_XX}\"${c_fg}\)${c_15}\)${c_02} / ${c_05}%c ${c_13}%C${c_XX}\))"
}
function i_ac_load {
  BAT=${(M)$(acpi -b)#*%}
  BAT=${c_01}${${${${BAT// /}/:/ }/\%/${c_09}\%}%:*}
  print "${c_XX}\(${c_04}= ${c_09}"${${${${${${${${(s. .)$(</proc/loadavg)}[1]/0./${c_08}0.}/1./${c_07}1.}/2./${c_05}2.}/3./${c_04}3.}/4./${c_03}4.}/5./${c_11}5.}//./${c_12}.${c_XX}}"\
 ${c_07}\(${c_05}rtl ${c_fg}\(${c_04}/ ${BAT}${c_fg}\)${c_07}\)${c_XX}\)"
}
function i_mixer {
  VOLUME=${${$(pulsevol -a volume)/0:/${c_07}L:${c_XX}}/1:/${c_07}R:${c_XX}}
  MUTE="$(pulsevol -a mute)"
  print "${c_07}\(${c_04}setf ${c_05}*pulseaudio* ${c_06}\'${c_fg}\(${c_07}${VOLUME}${c_fg}\) ${c_07}${${MUTE/no/:NIL}/yes/:T}${c_07})${c_XX}"
}
function i_date {
  print "${c_XX}\(${c_05}cons ${c_XX}\"${c_07}$(date "+%Y${c_08}.${c_XX}%m${c_08}.${c_XX}%d${c_02}/${c_12}%a${c_XX}\" \"${c_04}%H%M"${c_XX}\")\)"
}
function o_right { print "$(i_ac_load) $(i_newmail) $(i_date)" }


##+ RUNTIME OUTPUT:
while true; do
  rwidth=$(( $(print ${#${"$(o_right | sed 's.\^[^(]*([^)]*)..g')"}}) * 6 + 12))
  print "${c_XX} :${o_name}  ${c_XX}$(i_mifo)${c_XX} $(i_mixer)${c_XX}^pa($((1590 - $rwidth)))$(o_right)"
  sleep 1s
done | ${o_dzen} -u -x ${o_x} -y ${o_y} -h ${o_height} -w ${o_width} -ta 'l' -bg ${o_bg} -fg ${o_fg} -fn ${o_font} &
