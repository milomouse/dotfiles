#!/bin/zsh
banished=/home/milo/.config/.banishmouse
[ ! -f $banished ] || rm -f $banished
banishmouse
#xbindkeys &
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 135 = Alt_R"
xmodmap -e "keycode 64 = Super_L"
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "clear Mod1"
xmodmap -e "clear Mod4"
xmodmap -e "add Mod1 = Alt_L Alt_R"
xmodmap -e "add Mod4 = Super_L Super_R"
xsetroot -cursor_name left_ptr &
#wallie &
display -window root -resize $(xdpyinfo|grep dimensions|awk '{print $2}')! \
/home/milo/foto/wall/wallpaper-249719.jpg
#exec musca -s /home/milo/.config/musca/musca_start >&/dev/null
#exec catwm >&/dev/null
exec xmonad >&/dev/null
