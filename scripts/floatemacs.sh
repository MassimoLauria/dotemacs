#!/bin/sh

MYTERM=xterm
CLASSNAME=floatemacs

EDITOR="emacsclient -t"
ALTEDITOR=qemacs


GEOMETRY=90x50+550+0

exec $MYTERM \
    -name "$CLASSNAME" \
    -geom "$GEOMETRY" \
    -e $EDITOR -a $ALTEDITOR \
    $@
