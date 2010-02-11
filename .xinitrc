#!/bin/zsh
## ~/.xinitrc
## milomouse

# delete leftover 'mouv' file if present
banished=$HOME/.config/.mouv
if [ -f $banished ]; then
  rm $banished
fi
# swap alt_l with super_l
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 64 = Super_L"
display -window root ~/foto/wall/black_mosaic_by_seikq.jpg
# start window manager
exec ratpoison >> /dev/null
