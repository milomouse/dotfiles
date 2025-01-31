#! /bin/bash
# quick and dirty script to toggle between monitors
# also has a sanity check for switching to a monitor during herbstluftwm's autostart

if [[ "$1" == toggle ]]; then
    for i in $(herbstclient tag_status); do
        # check for "focused" tag on a different monitor
        if [[ ${i:0:1} == "-" ]] ; then
            herbstclient use "${i:1}"
            exit 0
        # check for "urgent" tag on a different monitor
        elif [[ ${i:0:1} == "!" ]] ; then
            herbstclient use "${i:1}"
            exit 0
        fi
    done
elif [[ "$1" == switch ]]; then
    if [[ -n $2 ]]; then
        # if we are not focused on specified monitor then switch to it
        if [[ $(herbstclient list_monitors | grep FOCUS | cut -d ':' -f1) != $2 ]]; then
            herbstclient focus_monitor "$2"
            exit 0
        fi
        # otherwise do nothing (useful for herbstluftwm restarts)
    else
        echo "you must specify which monitor to focus"
        exit 1
    fi
else
    echo "usage: $0 [switch <monitor>] || [toggle]"
    exit 0
fi
