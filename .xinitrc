#!/bin/zsh
banished=/home/milo/.config/.banishmouse
[ ! -f $banished ] || rm -f $banished
banishmouse
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 135 = Alt_R"
xmodmap -e "keycode 64 = Super_L"
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "clear Mod1"
xmodmap -e "clear Mod4"
xmodmap -e "add Mod1 = Alt_L Alt_R"
xmodmap -e "add Mod4 = Super_L Super_R"
xsetroot -cursor_name left_ptr &
. ~/.xmonad/dzen2start
display -window root /home/milo/foto/wall/black_mosaic_by_seikq.jpg
exec xmonad >&/dev/null
