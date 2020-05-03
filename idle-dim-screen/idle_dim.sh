#!/usr/bin/env bash

# check if the system is idle and dim the screen
# requirements: brew install brightness

checkInterval=60   # seconds
idleThreshold=10   # seconds
idleBrightness=0   # percent
wakeBrightness=0.5 # percent

while :
do
    # check the idle time
    idleSec=$(/usr/sbin/ioreg -c IOHIDSystem | /usr/bin/awk '/HIDIdleTime/ {print int($NF/1000000000); exit}')

    if  [[ "$idleSec" -gt $idleThreshold ]] ; then
        # idle since 60 seconds, dim display
        echo "idling ($idleSec sec), dim display to $idleBrightness"
        brightness $idleBrightness

        # alternative:
        # https://www.maketecheasier.com/adjust-screen-brightness-from-terminal-macos/
        # osascript -e 'tell application "System Events"' -e 'key code 145' -e ' end tell' # repeat x times for dark
    else
        echo "not idling ($idleSec sec)"
        # we are not idle, check if screen is already on again
        brightnessLevel=$(brightness -l | awk 'END {print $NF}')
        if [[ $(echo "$brightnessLevel < 0.1" | bc) == 1 ]] ; then
            echo "setting brightness to $wakeBrightness"
            # screen is on but brightness level is below 10%, set it to 50%
            brightness $wakeBrightness
        fi
    fi
    sleep $checkInterval
done