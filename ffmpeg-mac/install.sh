#!/bin/bash

set -e

snapshot="https://evermeet.cx/ffmpeg/get/zip"
stable="https://evermeet.cx/ffmpeg/getrelease/zip"

dlURL=$stable

if [ "$1" == "snapshot" ]; then
    dlURL=$snapshot
    printf "(snapshot) "
else 
    printf "(stable) "
fi

printf "downloading... "
curl $dlURL -JL -s -o ffmpeg.zip

# TODO: check signature
# printf "verify signature... "
# curl "$dlURL/sig" -JL -s -o ffmpeg.zip.sig
# gpg --verify ffmpeg.zip.sig ffmpeg.zip

printf "extracting... "
unzip -q ffmpeg.zip

printf "installing... "
mv ffmpeg /usr/local/bin/

printf "cleaning up... "
rm ffmpeg.zip
rm ffmpeg.zip.sig

printf "finished!\n"