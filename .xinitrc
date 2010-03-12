#!/bin/zsh
## ~/.xinitrc
banished=$HOME/.config/.mouv
[ ! -f $banished ] || rm $banished
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 64 = Super_L"
xbindkeys &
#display -window root ~/foto/wall/carrv.jpg
exec ion3plus -noerrorlog >>/dev/null
