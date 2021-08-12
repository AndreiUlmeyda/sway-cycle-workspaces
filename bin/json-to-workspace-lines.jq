#!/usr/bin/jq -fr

# Take a JSON encoded description of the current workspace layout
# as obtained by the command 'swaymsg --raw --type get_workspaces' like
#
#[
#  {
#    "id": 4,
#    "type": "workspace",
#    ...
#    "layout": "splith",
#    "border": "none",
#    "current_border_width": 0,
#    ...
#    "deco_rect": {
#      ...
#    },
#    "window_rect": {
#      ...
#    },
#    "geometry": {
#      ...
#    },
#    "name": "1",
#    ...
#    "output": "eDP-1",
#    "representation": "H[Alacritty]",
#    "focused": true,
#    "visible": true
#  },
#  {
#    ...
#    "name": "2",
#    "output": "eDP-1",
#    "focused": false,
#    ...
#  }
#]
#
# to a simpler line format like
#
# "eDP-1 1 true"
# "eDP-1 2 false"
#
# preserving only the information needed to determine the next or previous workspace

.[] | "\(.output) \(.name) \(.focused)"