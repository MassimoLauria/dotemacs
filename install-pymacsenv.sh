#!/bin/sh
#
# Copyright (C) 2010, 2011 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2011-03-06, domenica 11:52 (CET) Massimo Lauria"
# Time-stamp: "2011-03-07, lunedì 11:28 (CET) Massimo Lauria"

# Description::
#
# Install a python environment suitable for the last version of Pymacs and
# Ropemacs. Everything in user space.

# Code::

#URLs and VERSIONs
PYMACS=https://github.com/pinard/Pymacs/
PYMACSVER=v0.24-beta2

# Paths
EMACSD=$HOME/.emacs.d/
PYENV=$EMACSD/local-python
OLD_PWD=$PWD

# Programs
cd $(dirname $0)
VENV=$PWD/virtualenv.py


# Utilities
MKTEMP=mktemp
MKDIR=mkdir
RM=rm
CP=cp
MAKE=make
GIT=git
HG=hg


# ------------------- Functions -----------------------------
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


find_mercurial_avoid_sagemath() {

    which grep >/dev/null 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "grep is not present. Can't test Mercurial to avoid sagemath installation."
        return 2
    fi

    for hg in `which hg` "/usr/bin/hg" "/usr/local/bin/hg"; do
        grep sage "$hg" >/dev/null
        if [ $? -ne 0 -a -x $hg ]; then
            echo "A useful Mercurial installation was found at $hg"
            export HG=$hg
            return 0
        fi
    done
    return 2
}


find_mercurial_avoid_sagemath

require_program $GIT
require_program $HG
require_program $CP
require_program $RM
require_program $MAKE
require_program $MKDIR
require_program "python"


# Setup the temporary file location
which  $MKTEMP 2> /dev/null 1> /dev/null
if [ $? -eq 0 ]; then
    TEMPDIR=`mktemp -d`
else
    TEMPDIR="/tmp/ropetemp"
    $MKDIR -p $TEMPDIR
fi

echo "Setting up temp location $TEMPDIR"
cd $TEMPDIR


# Setup the environment
$RM -fr $PYENV
python $VENV $PYENV
. $PYENV/bin/activate

# Download and Setup Rope
$HG clone http://bitbucket.org/agr/rope
$HG clone http://bitbucket.org/agr/ropemacs
easy_install rope
easy_install ropemacs

# Download and Setup Pymacs
$GIT clone $PYMACS
cd Pymacs
$GIT co $PYMACSVER
$MAKE
easy_install .
$RM -f $EMACSD/pymacs.el
$CP pymacs.el $EMACSD

# Cleanup the mess
cd $OLD_PWD
$RM -fr $TEMPDIR

echo "*******************************************************************"
echo "*  Add the following line to your init.el file                    *"
echo "*                                                                 *"
echo '* (setenv "PYMACS_PYTHON" "~/.emacs.d/local-python/bin/python")   *'
echo "*******************************************************************"

exit 0

# Local Variables:
# fill-column: 80
# End:
