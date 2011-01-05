if [[ $TERM == linux || $TERM =~ rxvt ]]; then
  cd ${H:-/howl}
elif [[ $TERM =~ screen && -n $DISPLAY ]]; then
  cd ${H:-/howl}
fi
