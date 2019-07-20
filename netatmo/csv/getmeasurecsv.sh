#!/bin/bash

# https://www.michaelmiklis.de/export-netatmo-weather-station-data-to-csv-excel/
USER=$1
PASS=$2
DEVICE=$3
MODULE=$4
SENSORS=$5
DATE=$6 # e.g. 2015-05-17
NAME=$7

getmeasurecsv() {
    # ------------------------------------------------------
    # Help
    # ------------------------------------------------------
    # usage: getmeasurecsv <USER> <PASSWORD> <DEVICE_ID> <MODULE_ID> <TYPE> <STARTDATE> <ENDDATE> <FORMAT>
    #
    # USER + PASSWORD -> your NetAtmo Website login
    # DEVICE_ID -> Base Station ID
    # MODULE_ID -> Module ID
    # TYPE -> Comma-separated list of sensors (Temperature,Humidity,etc.)
    # STARTDATE -> Begin export date in format YYYY-mm-dd HH:MM
    # ENDDATE -> End export date in format YYYY-mm-dd HH:MM
    # FORMAT -> csv or xls
  
    # ------------------------------------------------------
    # Parsing Arguments
    # ------------------------------------------------------
    USER=$1
    PASS=$2
  
    DEVICE_ID=$3
    MODULE_ID=$4
    TYPE=$5
    DATETIMEBEGIN=$6
    DATETIMEEND=$7
    FORMAT=$8
    
    MODULE_NAME=$9
  
    # ------------------------------------------------------
    # Define some constants
    # ------------------------------------------------------
    URL_LOGIN="https://auth.netatmo.com/en-us/access/login"
    URL_POSTLOGIN="https://auth.netatmo.com/access/postlogin"
    API_GETMEASURECSV="https://api.netatmo.com/api/getmeasurecsv"
    SESSION_COOKIE="cookie_sess.txt"
  
  
    # ------------------------------------------------------
    # Convert start and end date to timestamp
    # ------------------------------------------------------
    DATEBEGIN="$(date --date="$DATETIMEBEGIN" "+%d.%m.%Y")"
    TIMEBEGIN="$(date --date="$DATETIMEBEGIN" "+%H:%M")"
    DATE_BEGIN="$(date --date="$DATETIMEBEGIN" "+%s")"
    DATEEND="$(date --date="$DATETIMEEND" "+%d.%m.%Y")"
    TIMEEND="$(date --date="$DATETIMEEND" "+%H:%M")"
    DATE_END="$(date --date="$DATETIMEEND" "+%s")"
  
  
    # ------------------------------------------------------
    # URL encode the user entered parameters
    # ------------------------------------------------------
    USER="$(urlencode $USER)"
    PASS="$(urlencode $PASS)"
    DEVICE_ID="$(urlencode $DEVICE_ID)"
    MODULE_ID="$(urlencode $MODULE_ID)"
    TYPE="$(urlencode $TYPE)"
    DATEBEGIN="$(urlencode $DATEBEGIN)"
    TIMEBEGIN="$(urlencode $TIMEBEGIN)"
    DATEEND="$(urlencode $DATEEND)"
    TIMEEND="$(urlencode $TIMEEND)"
    FORMAT="$(urlencode $FORMAT)"
  
  
    # ------------------------------------------------------
    # Now let's fetch the data
    # ------------------------------------------------------
  
    # get token from hidden <input> field
    TOKEN="$(curl --silent -c $SESSION_COOKIE $URL_LOGIN | sed -n '/token/s/.*name="_token"\s\+value="\([^"]\+\).*/\1/p')"
 
    # and now we can login using cookie, id, user and password
    curl --silent -d "_token=$TOKEN&email=$USER&password=$PASS" -b $SESSION_COOKIE -c $SESSION_COOKIE $URL_POSTLOGIN > /dev/null
 
    # next we extract the access_token from the session cookie
    ACCESS_TOKEN="$(cat $SESSION_COOKIE | grep netatmocomaccess_token | cut -f7)"
  
    # build the POST data
    PARAM="access_token=$ACCESS_TOKEN&device_id=$DEVICE_ID&type=$TYPE&module_id=$MODULE_ID&scale=max&format=$FORMAT&datebegin=$DATEBEGIN&timebegin=$TIMEBEGIN&dateend=$DATEEND&timeend=$TIMEEND&date_begin=$DATE_BEGIN&date_end=$DATE_END"
  
    # now download data as csv
    curl --silent -o "exports/$DATE-$MODULE_NAME.csv" -d $PARAM $API_GETMEASURECSV
  
    # clean up
    rm $SESSION_COOKIE
}
  
#____________________________________________________________________________________________________________________________________
  
urlencode() {
    # ------------------------------------------------------
    # urlencode function from mrubin
    # https://gist.github.com/mrubin
    #
    # usage: urlencode <string>
    # ------------------------------------------------------
    local length="${#1}"
  
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
  
        case $c in [a-zA-Z0-9.~_-])
            printf "$c" ;;
            *) printf '%%%02X' "'$c"
            esac
    done
}
  
#____________________________________________________________________________________________________________________________________

echo "get $DATE $NAME"
getmeasurecsv "$USER" "$PASS" "$DEVICE" "$MODULE" "$SENSORS" "$DATE 00:00:00" "$DATE 23:59:59" "csv" "$NAME"
