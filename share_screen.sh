#!/bin/sh
# This script uses ffmpeg to stream main monitor to the interactive learning system on linux systems.
# Obviously ITS can't make the universities learning system compatible with linux...

if [ "$#" -ne 1 ]; then
    echo "USAGE: $0 <ip address>" >&2
    exit 1
fi


ffmpeg -f x11grab -i :0 -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -tune zerolatency -r 30 -b:v 1000k -minrate 1000k -maxrate 1000k -f mpegts "tcp://$1:50921"