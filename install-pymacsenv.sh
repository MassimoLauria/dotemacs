
#!/bin/sh
#
# Copyright (C) 2010, 2011 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2011-03-06, domenica 11:52 (CET) Massimo Lauria"
# Time-stamp: "2012-01-30, 00:41 (CET) Massimo Lauria"

# Description::
#
# Install a python environment suitable for Pymacs and other several packages,
# in order not to rely on the system environment.

# Code::

# Python program and packages to be installed
PYTHON=python
PKGS="pyflakes pylint pep8 rope ropemode ipython cython readline"

# Pymacs version
PYMACS=https://github.com/pinard/Pymacs/
PYMACSVER=v0.24-beta2

# Paths
EMACSD=$HOME/.emacs.d/
PYENV=$EMACSD/local-python
OLD_PWD=$PWD

# Programs
cd $(dirname $0)
VENV=$PWD/virtualenv.py



# Setup the basic environment
echo "[1/3] Setup of basic environment."
rm -fr $PYENV
$PYTHON $VENV --clear --no-site-packages $PYENV
. $PYENV/bin/activate

# Install all packages (may require some ti)
echo "[2/3] Packages installation (wait several minutes)."
$PYENV/bin/pip install $PKGS


# Install
echo "[3/3] Install Pymacs."
git clone $PYMACS
cd Pymacs
git  co $PYMACSVER
make
easy_install .
rm -f $EMACSD/pymacs.el
cp pymacs.el $EMACSD
cd ..
rm -fr Pymacs

exit 0

# Local Variables:
# fill-column: 80
# End:
