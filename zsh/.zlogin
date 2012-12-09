command clear
if [[ ${#PATH/local} != ${#PATH} ]] { PATH=${PATH}:/usr/local/sbin:/usr/local/bin }
PATH="/usr/share/perl5/vendor_perl/auto/share/dist/Cope:${PATH}"
if [[ ${TERM} == linux || ${TERM} =~ rxvt || ${TERM} =~ screen && ${+DISPLAY} == 1 ]] { cd ${H:-/howl} }
if [[ ! -S /tmp/user-keep/${USER}/tmux/xorg ]] { command tmux -qS /tmp/user-keep/${USER}/tmux/xorg start-server }
if [[ ! -S /tmp/user-keep/${USER}/tmux/default ]] { command tmux -qS /tmp/user-keep/${USER}/tmux/default start-server }
