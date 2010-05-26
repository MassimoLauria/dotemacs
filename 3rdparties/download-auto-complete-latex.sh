#!/bin/sh

# Initial clone/download of auto-complete-latex, which is on 
# a mercurial repository, instead of a git one.  Thus i can't add
# it as a submodule.

if [ ! -f $PWD/download-auto-complete-latex.sh ]; then
    echo "Please run the script in its own directory"
    exit 1
fi

if [ -d auto-complete-latex/.hg ]; then
    echo "auto-complete-latex is already a Mercurial repository. You may want to run 'hg update' there."
    exit 1
fi

echo "Remove unversioned auto-complete-latex..."
rm -fr ./auto-complete-latex
echo "Download auto-complete-latex from Mercurial repository"
hg clone http://bitbucket.org/tequilasunset/auto-complete-latex
echo "Bye bye"
