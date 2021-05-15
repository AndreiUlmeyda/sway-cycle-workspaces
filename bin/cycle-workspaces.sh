#!/bin/bash

set -e
set -o pipefail

swaymsg -t get_workspaces | ./workspaces-per-monitor.jq | ./cycle-workspaces-exe "$1" | xargs swaymsg workspace number