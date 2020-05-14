#!/usr/bin/env bash

# check if the system is idle and dim the screen
# requirements: brew install brightness

checkInterval=1   # seconds
idleThreshold=20   # seconds
idleBrightness=0   # percent
wakeBrightness=0.5 # percent
state="bright"
lowered=0

while :
do
    # check the idle time
    idleSec=$(/usr/sbin/ioreg -c IOHIDSystem | /usr/bin/awk '/HIDIdleTime/ {print int($NF/1000000000); exit}')

    if  [[ "$idleSec" -gt $idleThreshold ]] ; then
        if [ $state != "dark" ] ; then
          # idle since 60 seconds, dim display
              echo "idling ($idleSec sec), dim display to $idleBrightness"
             # brightness $idleBrightness

            # alternative:
             # https://www.maketecheasier.com/adjust-screen-brightness-from-terminal-macos/
            osascript -e 'tell application "System Events"' -e 'key code 145' -e ' end tell' # repeat x times for dark

            ((lowered++))

            if [ $lowered -ge 16 ]; then
                state="dark"
            fi
        fi
    else 
        if [ $state != "bright" ]; then
            echo "not idling ($idleSec sec)"
            lowered=0
            # we are not idle, check if screen is already on again
            #brightnessLevel=$(brightness -l | awk 'END {print $NF}')
              # if [[ $(echo "$brightnessLevel < 0.1" | bc) == 1 ]] ; then
                 # echo "setting brightness to $wakeBrightness"
                # screen is on but brightness level is below 10%, set it to 50%
                # brightness $wakeBrightness

            for i in {1..12}
            do
                 osascript -e 'tell application "System Events"' -e 'key code 144' -e ' end tell' # repeat x times for dark
            done
            state="bright"
        fi
    fi
    sleep $checkInterval
done