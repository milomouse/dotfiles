#!/bin/bash

# The name of polybar bar which houses the main spotify module and the control modules.
# PARENT_BAR="main"
# PARENT_BAR_PID=$(pgrep -a "polybar" | grep "$PARENT_BAR" | cut -d" " -f1)
PARENT_BAR_PID=$(pgrep -x "polybar")

# Format of the information displayed
# See more attributes here: https://github.com/altdesktop/playerctl/#printing-properties-and-metadata

# Sends $2 as message to all polybar PIDs that are part of $1
update_hooks() {
    while IFS= read -r id
    do
        polybar-msg -p "$id" action "#spotify-$3.hook.$2" 1>/dev/null 2>&1
    done < <(echo "$1")
}

PLAYERCTL_STATUS=$(playerctl --player=spotify status 2>/dev/null)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    STATUS=$PLAYERCTL_STATUS
    FORMAT=$(playerctl --player=spotify metadata --format "{{ shuffle }} {{ loop }} {{ artist }} - {{ title }}" 2>/dev/null)
else
    STATUS="No player is running"
fi

if [ "$1" == "--status" ]; then
    echo "$STATUS"
else
    if [ "$STATUS" = "No player is running"  ]; then
        update_hooks "$PARENT_BAR_PID" 0 state
        update_hooks "$PARENT_BAR_PID" 0 shuffle
        update_hooks "$PARENT_BAR_PID" 0 loop
        echo ""
        exit
    elif [ "$STATUS" = "Paused"  ]; then
        update_hooks "$PARENT_BAR_PID" 1 state
    else
        update_hooks "$PARENT_BAR_PID" 2 state
    fi
    # set hook for Shuffle status
    if [ "$(echo $FORMAT | cut -d ' ' -f1)" == "false" ] ; then
      update_hooks "$PARENT_BAR_PID" 1 shuffle
    else
      update_hooks "$PARENT_BAR_PID" 2 shuffle
    fi
    # set hook for Loop status
    if [ "$(echo $FORMAT | cut -d ' ' -f2)" == "Track" ] ; then
      update_hooks "$PARENT_BAR_PID" 1 loop
    elif [ "$(echo $FORMAT | cut -d ' ' -f2)" == "Playlist" ] ; then
      update_hooks "$PARENT_BAR_PID" 2 loop
    elif [ "$(echo $FORMAT | cut -d ' ' -f2)" == "None" ] ; then
      update_hooks "$PARENT_BAR_PID" 3 loop
    fi
    # output command
    echo -n $FORMAT | cut -d ' ' -f1-2 --complement
fi
