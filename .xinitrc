#!/bin/zsh
## ~/.xinitrc
## milomouse

banished=$HOME/.config/.mouv
[ ! -f $banished ] || rm $banished
synap
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 64 = Super_L"
xbindkeys &
#exec ratpoison >> /dev/null
#exec ion3plus -noerrorlog >> /dev/null
exec musca -s $XDG_CONFIG_DIR/musca/musca_start >> /dev/null
