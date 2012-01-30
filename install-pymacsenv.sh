#!/bin/sh
#
# Copyright (C) 2010, 2011, 2012 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2011-03-06, domenica 11:52 (CET) Massimo Lauria"
# Time-stamp: "2012-01-30, 01:14 (CET) Massimo Lauria"

# Description::
#
# Install a python environment suitable for Pymacs and other several packages,
# in order not to rely on the system environment.

# Code::


# ---- Packages to be installed --------------------------------------
PKGS="pyflakes pylint pep8 rope ropemode ipython cython readline"

PYMACS=https://github.com/pinard/Pymacs/   # Pymacs must be downloaded
PYMACSVER=v0.24-beta2

# ---- Installation paths --------------------------------------------
EMACSD=$HOME/.emacs.d/
PYENV=$EMACSD/local-python




# ---- Other env variables -------------------------------------------
CP=cp
LN=ln
RM=rm
MKDIR=mkdir
FILE_NOT_FOUND=127

GIT=git
PYTHON=python

# Virtual env script
OLD_PWD=$PWD

cd $(dirname $0)
VENV=$PWD/virtualenv.py



# -- 1 -- Setup the basic environment --------------------------------
echo "[1/3] Setup of basic environment."
$RM -fr $PYENV
$PYTHON $VENV --clear --no-site-packages $PYENV 2>/dev/null

if [ $? -ne 0 ]; then
    echo "Some problems during setup. Aborting."
    exit 1
fi


# -- 2 -- Install all packages ---------------------------------------
echo "[2/3] Packages installation (wait several minutes)."
. $PYENV/bin/activate
$PYENV/bin/pip install $PKGS


# -- 3 -- Install pymacs ---------------------------------------------
echo "[3/3] Install Pymacs."


$GIT clone $PYMACS
cd $(basename "$PYMACS")
$GIT co $PYMACSVER

make
easy_install .

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
