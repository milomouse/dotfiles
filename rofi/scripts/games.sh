#!/bin/bash
CONFIG="${XDG_CONFIG_DIR}/rofi/menu-game.rasi"
MAP="${XDG_CONFIG_DIR}/rofi/menus/games"

cat "$MAP" \
    | cut -d ',' -f 1 \
    | rofi -dmenu -i -config "${CONFIG}" -sort -p "play" \
    | head -n 1 | xargs -i --no-run-if-empty grep "{}" "$MAP" \
    | cut -d ',' -f 2 | head -n 1 | xargs -i --no-run-if-empty /bin/bash -c "{}"

exit 0
