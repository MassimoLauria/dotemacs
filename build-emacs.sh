#!/bin/sh

# Basic build setup
RELEASE=emacs-29.4
BUILDOPTS="--with-native-compilation --with-x-toolkit=lucid --with-mailutils --with-harfbuzz --with-json --with-tree-sitter"
PREFIX=${HOME}/.local
CONFDIR=${HOME}/config/emacs
INITFILE=${HOME}/.emacs.d/init.el
CC=gcc-12


# Package install under Debian/Ubuntu
# sudo apt build-dep emacs
# sudo apt install libgccjit0 libgccjit-XX-dev libjansson-dev

#
SRCDIR=${PREFIX}/src/emacs

if [ -d ${SRCDIR} ]; then
    echo "* Run: updating emacs sources"
    cd ${SRCDIR}
    git fetch -a > /dev/null
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
cd lisp
make autoloads
cd ..
make -j4

EMACS_VERSION=`${SRCDIR}/src/emacs --batch -Q --eval '(print emacs-version)'|xargs`

echo "* Run: install version ${EMACS_VERSION} in ${PREFIX}"
make install

cd ${CONFDIR}

echo "* Run: setup the configuration"
EMACS=${SRCDIR}/bin/emacs-${EMACS_VERSION} make
