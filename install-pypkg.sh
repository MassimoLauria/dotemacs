#!/bin/sh
#
# Copyright (C) 2010, 2011, 2012 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2011-03-06, domenica 11:52 (CET) Massimo Lauria"
# Time-stamp: "2012-05-25, 14:24 (CEST) Massimo Lauria"

# Description::
#
# Install python packages required for Emacs in the user python modules path.

# Code::


# ---- Packages to be installed --------------------------------------
PKGS="rope ropemode ropemacs"
#PKGS="pyflakes pylint pep8 rope ropemode ropemacs"
PYMACSREPO=git://github.com/pinard/Pymacs.git
PYMACS=Pymacs
#PYMACSVER=v0.24-beta2
PYMACSVER=v0.25

# ---- Env variables -------------------------------------------
CP=cp
LN=ln
RM=rm
MKDIR=mkdir
FILE_NOT_FOUND=127

GIT=git
PYTHON=python
PYPIP=pip
PYEIN=easy_install

EMACSD=$HOME/.emacs.d/

# Virtual env script
OLD_PWD=$PWD


# ------------------- Utilities -----------------------------
require_program()
{
    if [ $# -ne 1 ]; then echo "Wrong argument number."; exit 1; fi

    echo -n "Checking for '$1'... "
    which $1 2> /dev/null > /dev/null
    if [ $? -ne 0 ]; then
        echo "FAIL."
        exit_on_missing_program $1
    else
        echo "OK."
    fi
}

exit_on_missing_program() {
    echo ""
    echo "Unfortunately program \"$1\" is not present, or it is not executable."
    echo "Without this software, I can't finish the installation."
    echo ""
    echo "Bye bye."
    exit $FILE_NOT_FOUND
}


# -- 1 -- Setup the basic environment --------------------------------
echo "[1/3] Check for the presence of some programs:"
require_program $RM
require_program $GIT
require_program $PYEIN
require_program $PYPIP
echo ""
echo ""

# -- 2 -- Install all packages ---------------------------------------
echo "[2/3] Packages installation (please wait)."
$PYPIP install --user $PKGS

if [ $? -ne 0 ]; then
    echo "Some problems during module installation. Aborting."
    exit 1
fi

# -- 3 -- Install pymacs ---------------------------------------------
echo "[3/3] Install Pymacs."

$GIT clone $PYMACSREPO
cd $(basename "$PYMACS")
$GIT co $PYMACSVER
make
$PYEIN --user .

$RM -f $EMACSD/pymacs.el
$CP pymacs.el $EMACSD
cd ..
$RM -fr $(basename "$PYMACS")


# Back to where we were...
cd $OLD_PWD
exit 0


# Local Variables:
# fill-column: 80
# End:
