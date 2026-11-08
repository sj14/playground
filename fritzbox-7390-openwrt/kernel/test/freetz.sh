# Shell access to the box under Freetz, through the shell page of its web
# interface. Source this file. FREETZ_URL defaults to http://192.168.0.2:81,
# FREETZ_AUTH (user:password) is required.
FREETZ_URL=${FREETZ_URL:-http://192.168.0.2:81}
: "${FREETZ_AUTH:?set FREETZ_AUTH to user:password of the Freetz web interface}"

# The shell page hands out a token for each command.
freetz_token() {
    curl -s --max-time 30 -u "$FREETZ_AUTH" "$FREETZ_URL/cgi-bin/shell/index.cgi" \
        | grep -oE 'cmd\.cgi\?pid=[0-9]+' | head -1 | cut -d= -f2
}

# Runs a local script file on the box and prints its output. The shell page cuts
# the output after 64,000 bytes.
freetz_run() {
    curl -s --max-time 120 -u "$FREETZ_AUTH" --data-urlencode "script@$1" \
        "$FREETZ_URL/cgi-bin/shell/cmd.cgi?pid=$(freetz_token)" \
        | sed -E 's/&lt;/</g; s/&gt;/>/g; s/&quot;/"/g; s/&amp;/\&/g'
}

# Copies a local file to the box.
freetz_upload() {
    curl -s --max-time 60 -u "$FREETZ_AUTH" -F "source=@$1" -F "target=$2" \
        "$FREETZ_URL/cgi-bin/shell/upload.cgi?pid=$(freetz_token)" > /dev/null
}

freetz_is_up() {
    [ "$(curl -s -o /dev/null --max-time 2 -w '%{http_code}' -u "$FREETZ_AUTH" "$FREETZ_URL/")" = 200 ]
}

# Waits up to $1 seconds for the web interface.
freetz_wait() {
    local start
    start=$(date +%s)
    while [ $(( $(date +%s) - start )) -lt "$1" ]; do
        freetz_is_up && return 0
        sleep 2
    done
    return 1
}
