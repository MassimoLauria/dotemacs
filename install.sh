#!/bin/sh
#
# Copyright (C) 2010, 2011 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2011-03-05, sabato 01:03 (CET) Massimo Lauria"
# Time-stamp: "2011-03-07, lunedì 11:39 (CET) Massimo Lauria"

# Description::
#
# My Emacs configuration is rich and require some software to be
# present.  In some cases the configuration would gracefully ignore
# the missing software.

# It will also link the init.el file in the appropriate place.


# -------------------- Env Variables ------------------------
EMACSD=$HOME/.emacs.d

GIT=git
CP=cp
LN=ln
RM=rm
MKDIR=mkdir

FILE_NOT_FOUND=127


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

backup_maybe() {
# Check backup possibility.
    if [ $# -ne 1 ]; then echo "Wrong argument number."; exit 1; fi
    if [ -e $1 ]; then
        $CP -arb $1 $1.bak.`date +%Y-%m-%d.%H.%M.%S`
    fi
}

issue_warning_on_pwd() {
    if [ $# -ne 1 ]; then echo "Wrong argument number."; exit 1; fi
    if [ "$PWD" != "$HOME/$1" ]; then
        echo ""
        echo "WARNING: you are installing in a wrong path."
        echo "WARNING: the right path should be ~/$1."
        echo "WARNING: installation would go on, but something may not work properly."
        echo "WARNING: remember that moving folder requires running this script again."
        echo ""
    fi
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


# ------------------- Installation -------------------------

# Goto config folder.
cd $(dirname $0)


issue_warning_on_pwd "config/emacs"

echo "Check for the present of basic programs"
require_program $GIT
require_program $CP
require_program $LN
require_program $RM
echo ""
echo ""

echo "Create '$EMACSD' if not present..."
$MKDIR -p $EMACSD
echo "OK"

echo ""
echo "******************************************"
echo "        Install third party packages      "
echo "******************************************"
echo ""

echo "Install Git submodules"
$GIT submodule init
$GIT submodule update
echo ""
echo "DONE"
echo ""


#------------- Installing auto-complete-latex. A little bit dirty :-(
ACLTGZFILE=http://bitbucket.org/tequilasunset/auto-complete-latex/get/c936a026703b.tar.gz
ACLTGZDIR=tequilasunset-auto-complete-latex-c936a026703b

find_mercurial_avoid_sagemath

if [ -x "$HG"  ]; then

    # Check if it is possible to download the package
    ACLDOWNLOAD="yes"

    which "wget" 2> /dev/null > /dev/null
    if [ $? -ne 0 ]; then
        ACLDOWNLOAD="no"
    fi

    which "tar" 2> /dev/null > /dev/null
    if [ $? -ne 0 ]; then
        ACLDOWNLOAD="no"
    fi

    which "mv" 2> /dev/null > /dev/null
    if [ $? -ne 0 ]; then
        ACLDOWNLOAD="no"
    fi

    if [ "$ACLDOWNLOAD" = "yes" ]; then

        echo "Install 'auto-complete-latex' (from tag.gz file)"
        wget $ACLTGZFILE -O $PWD/ac-latex.tgz
        tar xvfz $PWD/ac-latex.tgz

        $RM    -fr  $PWD/3rdparties/auto-complete-latex/
        $MKDIR -p   $PWD/3rdparties/auto-complete-latex/

        mv $PWD/$ACLTGZDIR/* 3rdparties/auto-complete-latex/

        $RM    -fr  $PWD/ac-latex.tgz
        $RM    -fr  $PWD/$ACLTGZDIR/

        echo "DONE."
    else
        echo "Installation of 'auto-complete-latex' failed"
        $MKDIR -p   $PWD/3rdparties/auto-complete-latex/
    fi
else
    echo "Install 'auto-complete-latex' (from bitbucket.org with Mercurial)"
    $RM -fr   $PWD/3rdparties/auto-complete-latex/
    $HG clone \
        http://bitbucket.org/tequilasunset/auto-complete-latex \
        3rdparties/auto-complete-latex/
    echo "DONE"
fi
echo ""
echo ""


#------------- Update python documentation for pylookup
PYLOOKUP=$PWD/3rdparties/pylookup/pylookup.py
PYDOCDIR1=/usr/share/doc/python2.6-doc/html/
PYDOCDIR2=$PWD/3rdparties/pylookup/python-2.7.1-docs-html/
PYDOCURL=http://docs.python.org
PYDOCDB=$EMACSD/pylookup.db

if [ -x "$PYLOOKUP" ]; then
    if [ -d "$PYDOCDIR1" ]; then
        echo "Update python documentation DB (my local files)"
        PYLOOKUPURL=$PYDOCDIR1
    elif [ -d "$PYDOCDIR2" ]; then
        echo "Update python documentation DB (included files)"
        PYLOOKUPURL=$PYDOCDIR2
    else
        echo "Update python documentation DB (web)"
        PYLOOKUPURL=$PYDOCURL
    fi
    $PYLOOKUP -u $PYLOOKUPURL -d $PYDOCDB
else
    echo "Python documentation DB not installed."
fi

#------------- Update C/C++ documentation for cclookup
CCLOOKUP=$PWD/3rdparties/cclookup/cclookup.py
CCDOCDIR=$PWD/3rdparties/cclookup/www.cppreference.com/wiki/
CCDOCDB=$EMACSD/cclookup.db

#Test for a library which is necessary to CCLookup"
python -c "import BeautifulSoup" 2>/dev/null >/dev/null
if [ $? -ne 0 ]; then
    echo "CC Lookup depends on BeautifulSoup module, which is not present."
    echo "Please run 'sudo apt-get install python-beautifulsoup'"
    echo "CC Lookup installation failed."
elif [ -x "$CCLOOKUP" -a -d "$CCDOCDIR" ]; then
    echo "Update C/C++ documentation DB (included files)"
    $CCLOOKUP -u $CCDOCDIR -d $CCDOCDB
else
    echo "C/C++ documentation DB not installed (Either cclookup.py or doc files are missing)"
fi

# -------- Pymacs and Local Python environment ------------------
./install-pymacsenv.sh
if [ $? -eq 0 ]; then
    echo "Successful installation of Pymacs and Ropemacs in local python environment."
else
    echo "Failed installation pf Pymacs and Ropemacs"
fi

# -------------------- init.el ----------------------------------

echo "Everything was (more or less) smooth, now we install a new init.el file..."
backup_maybe $EMACSD/init.el
$RM -f $EMACSD/init.el
$LN -s $PWD/init.el $EMACSD/init.el
echo "OK."



# Local Variables:
# fill-column: 80
# End:
