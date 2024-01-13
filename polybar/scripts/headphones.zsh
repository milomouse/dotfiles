#! /bin/zsh
STATE=${$(amixer -c 1 | grep Headphone -A 5)[-1]}
if [[ "$STATE" == '[off]' ]] ; then
  echo " %{F#C2003B}%{F-} "
else
  echo " %{F#73B8A8}%{F-} "
fi
