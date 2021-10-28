#!/bin/sh

# Basic build setup
RELEASE=emacs-28
BUILDOPTS="--with-native-compilation --with-x-toolkit=lucid --with-mailutils --with-harfbuzz"
PREFIX=~/.local/
CC=gcc-10

#
SRCDIR=${PREFIX}/src/emacs

if [ -d ${SRCDIR} ]; then
    echo "* Run: updating emacs sources"
    cd ${SRCDIR}
    git pull > /dev/null
else
    echo "* Run: dowloading emacs sources"
    git clone git://git.sv.gnu.org/emacs.git ${SRCDIR}
    cd ${SRCDIR}
fi

echo "* Run: checkout version ${RELEASE}"
git checkout ${RELEASE}

if [ ! -f ./configure ]; then
    echo "* Run: autogen.sh"
    CC=${CC} ./autogen.sh
else
    echo "* Run: autogen.sh (SKIP)"
fi

if [ ! -f ./Makefile ]; then
    echo "* Run: configure"
    CC=${CC} ./configure ${BUILDOPTS} --prefix=${PREFIX}
else
    echo "* Run: configure (SKIP)"
fi

echo "* Run: build emacs"
make -j4

echo "* Run: install in ${PREFIX}"
