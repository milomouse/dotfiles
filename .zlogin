# autologin from `mingetty' (--autologin) then auto `startx' once from tty1
if [[ -z "${DISPLAY}" ]] && [[ $(tty) == /dev/tty1 ]] ; then
  startx
fi

# change directories from configuration dir to base dir
if [[ $PWD == $HOME ]] ; then
  cd ${H:-/home/${USER}}
fi
