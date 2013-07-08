#!/bin/zsh
## ${XDG_CONFIG_HOME:-$HOME}/bspm/dzen4bspwm.zsh
# dzen2 bar for bspwm (spawned and killed from xinitrc)


## output variables $
o_dzen=$(whence -p dzen2)
o_height='14'
o_width='1590'
o_x='5'
o_y='2'
o_font='-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r'
#o_font='-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1'
o_bg='#1a1a1a'
o_fg='#8c8b8e'
c_XX='^fg()'
c_00='^fg(#444444)'
c_01='^fg(#ba97c5)'
c_02='^fg(#7ecec4)'
c_03='^fg(#bfd2cb)'
c_04='^fg(#6da4ab)'
c_05='^fg(#ab9fc1)'
c_06='^fg(#80c4ac)'
c_07='^fg(#aaaaaa)'
c_08='^fg(#7c7c7c)'
c_09='^fg(#a488d9)'
c_10='^fg(#8d8d8d)'
c_11='^fg(#666666)'
c_12='^fg(#5f656b)'


## input functions $
function i_newmail {
  print - "\(${c_04}open ${c_XX}:unread ${c_06}\'${c_07}$(print - ${(Fw)#$(find /howl/mail/*/*/new -type f)})${c_XX})"
}
function i_mifo {
  m_a=$(mifo -a %B)
  [[ ${#${m_a}} -gt 50 ]] && m_N="${${m_a}[1,50]}.."
  printf "$(mifo -a \"${c_XX}\(${c_12}#${c_06}\'${c_07}\(${c_09}load ${c_10}\(${c_05}mplayer ${c_11}:dir ${c_XX}\"${c_04}%D:2:${c_XX}\" \
${c_11}:name ${c_XX}\"${c_06}${m_N:-%B}${c_XX}\" ${c_11}:type ${c_XX}\"${c_01}%e${c_XX}\"${c_10}\)${c_07}\)${c_04} / ${c_07}%c ${c_10}%C${c_XX}\))\""
}
function i_ac_load {
  BAT=${$(acpi -b)[-1]}
  BAT=${c_10}${${${${BAT// /}/:/ }/\%/${c_00}\%}%:*}
  print "${c_XX}\(${c_04}= "${${${${${${${(s. .)$(</proc/loadavg)}[1]/0./${c_00}0.}/1./${c_XX}1.}/2./${c_03}2.}/3./${c_02}3.}/4./${c_01}4.}//./${c_12}.${c_XX}}"\
 ${c_07}\(${c_05}rtl ${c_10}\(${c_04}/ ${BAT}${c_10}\)${c_07}\)${c_XX}\)"
}
function i_mixer {
  VOLUME=${${$(pulsevol -a volume)/0:/${c_11}L:${c_XX}}/1:/${c_11}R:${c_XX}}
  MUTE="$(pulsevol -a mute)"
  print "${c_07}\(${c_04}setf ${c_09}*pulseaudio* ${c_06}\'${c_03}\(${c_07}${VOLUME}${c_03}\) ${c_11}${${MUTE/no/:unmuted}/yes/:muted}${c_07})${c_XX}"
}
function i_date {
  print "${c_XX}\(${c_05}cons ${c_XX}\"${c_11}$(date "+%Y${c_00}.${c_XX}%m${c_00}.${c_XX}%d${c_02}/${c_09}%a${c_XX}\" \"${c_07}%H${c_08}%M"${c_XX}\")\)"
}
function o_right { print "$(i_ac_load) $(i_newmail) $(i_date)" }


## runtime output $
while true; do
  rwidth=$(( $(print ${#${"$(o_right | sed 's.\^[^(]*([^)]*)..g')"}}) * 6 + 12))
  #rwidth=$(( $(textwidth ${o_font} $(o_right)) * 10 ))
  print "${c_XX} :bspwm  ${c_XX}$(i_mifo)${c_XX} $(i_mixer)${c_XX}^pa($((1590 - $rwidth)))$(o_right)"
  #print "${c_XX} :bspwm ${c_XX}$(i_programs)${c_XX} $(i_ac_load)${c_XX} $(i_mifo) $(i_date)"
  sleep 1s
done | ${o_dzen} -u -x ${o_x} -y ${o_y} -h ${o_height} -w ${o_width} -ta 'l' -bg ${o_bg} -fg ${o_fg} -fn ${o_font} &
