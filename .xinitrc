#!/bin/zsh
banished=/home/milo/.config/.mouv
[ ! -f $banished ] || rm -f $banished
synap
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 64 = Super_L"
xbindkeys &
display -window root -geometry 1366x768! /home/milo/foto/wall/colorstripes.jpg
#exec musca -s $XDG_CONFIG_DIR/musca/musca_start >>/dev/null
exec ratpoison >>/dev/null
