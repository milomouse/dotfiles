#!/bin/zsh
banished=/home/milo/.config/.mouv
[ ! -f $banished ] || rm -f $banished
synap
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 64 = Super_L"
xbindkeys &
display -window root /home/milo/foto/wall/careview.jpg
exec musca -s /home/milo/.config/musca/musca_start >>/dev/null
