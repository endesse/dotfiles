#!/usr/bin/env bash

# Find the primary screen
#PRIMARY_MONITOR=$(xrandr | grep "primary" | awk '{print $1}')
PRIMARY_MONITOR=$(polybar --list-monitors | grep "primary" | awk '{print $1}' | cut -d":" -f1)

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

#for m in $(polybar --list-monitors | cut -d":" -f1); do	
#	if [ "$MONITOR" = "$PRIMARY_MONITOR" ]; then
#        	export TRAY_POS="right"
#    	fi
#
#	WIRELESS=$(ls /sys/class/net/ | grep ^wl | awk 'NR==1{print $1}') MONITOR=$m polybar --reload mainbar-xmonad -c ~/.config/polybar/config.ini &
#
#	unset TRAY_POS
#done

WIRELESS=$(ls /sys/class/net/ | grep ^wl | awk 'NR==1{print $1}') polybar --reload mainbar-xmonad -c ~/.config/polybar/config.ini &
polybar --reload secondary-xmonad -c ~/.config/polybar/config.ini &
#polybar --reload mainbar-xmonad-extra -c ~/.config/polybar/config.ini &
