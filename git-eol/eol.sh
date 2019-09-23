#!/bin/bash

##########################################
# dependencies: git, unix2dos, dos2unix
#
# usage: ./eol.sh lf|crlf path/to/files 
##########################################

set -e

conversion=$1
files=( "${@:2}" )

original_eol=$(git config core.eol)
original_autocrlf=$(git config core.autocrlf)

#git config core.eol "$conversion" # probably, this is not even necessary
git config core.autocrlf false

function reset_settings {
    git config core.eol "$original_eol"
    git config core.autocrlf "$original_autocrlf"
}

case "$conversion" in
    "lf")
        dos2unix "${files[@]}"
        ;;
    "crlf")
        unix2dos "${files[@]}"
        ;;
    *)
        echo "Wrong parameter '$conversion'. Use 'lf' or 'crlf'".
        reset_settings
        exit 1
esac

reset_settings
