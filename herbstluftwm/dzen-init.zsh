#! /bin/zsh
###########################################################
## locate: ${XDG_CONFIG_HOME}/herbstluftwm/dzen-init.zsh ##
## author: Vincent Z (github.com/milomouse)              ##
## detail: wm-independent information for `dzen2'        ##
###########################################################
## NOTE 1: needs to be started before `herbstluftwm'     ##
## NOTE 2: script should be ran from the "xinitrc" file  ##
## NOTE 3: will NOT be reloaded by `herbstclient reload' ##
###########################################################

source ${XDG_CONFIG_DIR:-$HOME}/herbstluftwm/dzen-colors.zsh

## FUNCTIONS:
function i_mifo {
  m_=$(mifo -a "%D:2: _MIFO1_ %D _MIFO2_ %B") ; [[ ${#m_} -eq 0 ]] && m_="/"
  m_A=${${m_/ _MIFO1_*}//_/ } ; [[ ${#m_A} -eq 0 ]] && m_A='<unknown>'
  m_D=${${${m_/ _MIFO2_*}/*_MIFO1_ }//_/ } ; [[ ${#m_D} -eq 0 ]] && m_D='<unknown>'
  [[ ${#${m_A}} -gt 20 ]] && m_A="${${m_A}[1,20]}.."
  [[ ${#${m_D}} -gt 20 ]] && m_D="${${m_D}[1,20]}.."
  [[ ${#${m_/*_MIFO2_ }} -gt 40 ]] && m_B="${${${m_/*_MIFO2_ }//_/ }[1,40]}.." || m_B="${${m_/*_MIFO2_ }//_/ }"
  print - "${c_XX}${b_08} ^bg(#333333)${c_07} ӎplayer2 ${c_08}${b_00}▒${_XX}\
$(mifo -a ${b_08} '^bg(#303030)'${c_12} ${m_A:-%D:2:} ${_XX}\
'^bg(#292929)^fg(#616161)' ${m_D:-%D} ${_XX}\
'^bg(#242424)'${c_13} ${m_B:-%B} ${_XX}\
'^bg(#292929)'${c_08} %e ${_XX}\
'^bg(#333333)'${c_04} %c ${c_XX}/ ${c_12}%C ${_XX})"
}

function i_mixer {
  VOLUME="${${${$(pulsevol -a volume)#0:}/1:/${c_07}ʆ^fg(#666666)}:gs/%/^fg(#484848)&}"
  MUTE="${${$(pulsevol -a mute)/yes/${c_01}}/no/${c_13}}•"
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) ⩗olume ${_XX}\
^fg(#666666)${b_00} ${VOLUME} ${_XX}\
^bg(#333333) ${MUTE} ${_XX}"
}

function i_newmail {
  INBOX=${(Fw)#$(find /howl/mail/*/INBOX/new -type f)}
  ALL=${(Fw)#$(find /howl/mail/*/*/new -type f)}
  print - "${c_XX}${b_08} ^bg(#333333)${c_07} ӎailbox ${c_08}${b_00}▒${_XX}\
${b_08} ^bg(#292929)${c_07} unread ${_XX}\
^bg(#242424)${c_05} ${INBOX} ${_XX}\
^bg(#292929)${c_13} ${ALL} ${_XX}"
}

function i_loadavg {
  LOAD=${${${${${${${${(s. .)$(</proc/loadavg)}[1]/0./${c_08}0.}/1./${c_07}1.}/2./${c_05}2.}/3./${c_04}3.}/4./${c_03}4.}/5./${c_11}5.}//./${c_12}.${c_XX}}
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) ɭoadavg ${_XX}\
^bg(#292929) ${LOAD} ${_XX}"
}

function i_battery {
  BAT=${${${${${${$(acpi -b)[3,4]//,}//\%/${c_07}% }/Full/${c_13} full ${c_12}${b_08}}/Discharging/${c_09} down ${c_12}${b_08}}/Charging/${c_03} up ${c_12}${b_08}}/Unknown/${c_08} pending ${c_02}${b_08}}
  print - "${c_XX}${b_08} ${c_07}^bg(#333333) Ϧattery ${_XX}\
^bg(#292929)${BAT}${_XX}"
}

## RUNTIME:
_font='-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1'
_wide='1270'
function left { print - "$(i_mifo) $(i_mixer)" }
function right { print - "$(i_newmail) $(i_loadavg) $(i_battery)"}

while true ; do
  _rwide=$(( $(print - ${#${"$(right | sed 's.\^[^(]*([^)]*)..g')"}}) * 6 + 2))
  print - "$(left)^pa($((${_wide} - ${_rwide})))$(right)"
  sleep 1s
done | dzen2 -p -x 330 -y 0 -h 16 -w ${_wide} -ta l -bg ${_bg} -fg ${_fg} \
       -fn '-misc-fixedzero-medium-r-semicondensed-*-12-110-75-75-c-60-iso10646-1' \
       &>/dev/null || exit 5
