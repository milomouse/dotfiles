command clear
if [[ ${#PATH/local} != ${#PATH} ]] { PATH=${PATH}:/usr/local/sbin:/usr/local/bin }
PATH="/usr/share/perl5/vendor_perl/auto/share/dist/Cope:${PATH}"
if [[ ${TERM} == linux || ${TERM} =~ rxvt || ${TERM} =~ screen && ${+DISPLAY} == 1 ]] { cd ${H:-/howl} }
#if [[ ${TERM} == linux && ${+DISPLAY} == 0 ]] { /usr/local/bin/welcome-arch }
if [[ ! -S /tmp/.${UID}/tmux/xorg ]] { command tmux -qS /tmp/.${UID}/tmux/xorg start-server }
if [[ ! -S /tmp/.${UID}/tmux/default ]] { command tmux -qS /tmp/.${UID}/tmux/default start-server }
