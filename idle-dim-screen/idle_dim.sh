#!/usr/bin/env bash

# check if the system is idle and dim the screen
# requirements: brew install brightness

checkInterval=1   # seconds
idleThreshold=20   # seconds
state="bright"

while :
do
    # check the idle time
    idleSec=$(/usr/sbin/ioreg -c IOHIDSystem | /usr/bin/awk '/HIDIdleTime/ {print int($NF/1000000000); exit}')

    if  [[ "$idleSec" -gt $idleThreshold ]] ; then
        if [ $state != "dark" ] ; then
              echo "idling ($idleSec sec), dim display"

            for i in {1..16}
            do
                osascript -e 'tell application "System Events"' -e 'key code 145' -e ' end tell'
            done

            state="dark"
        fi
    else 
        if [ $state != "bright" ]; then
            echo "not idling ($idleSec sec)"
            for i in {1..12}
            do
                 osascript -e 'tell application "System Events"' -e 'key code 144' -e ' end tell'
            done
            state="bright"
        fi
    fi
    sleep $checkInterval
done