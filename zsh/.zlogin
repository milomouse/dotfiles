if [[ $TERM == linux || $TERM =~ rxvt ]] || [[ $TERM =~ screen && -n $DISPLAY ]]; then
  cd ${H:-/howl}
fi
