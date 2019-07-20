#!/bin/bash

USER=$1
PASS=$2
LAST_X_DAYS=$3

while [ $LAST_X_DAYS -gt 0 ]
    do
    DATE=$(date -d "-$LAST_X_DAYS days" '+%Y-%m-%d')
    ./getmeasurecsv.sh $USER $PASS "70:ee:50:3f:03:d4" "70:ee:50:3f:03:d4" "Temperature,Humidity,CO2,Noise,Pressure" $DATE "INDOOR"
    ./getmeasurecsv.sh $USER $PASS "70:ee:50:3f:03:d4" "02:00:00:3f:6e:0e" "Temperature,Humidity" $DATE "OUTDOOR"
    ./getmeasurecsv.sh $USER $PASS "70:ee:50:3f:03:d4" "02:00:00:05:ed:f2" "Rain" $DATE "RAIN"
    LAST_X_DAYS=$((LAST_X_DAYS-1))
done