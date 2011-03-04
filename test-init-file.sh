#!/bin/sh

EMACS=emacs

$EMACS -q --eval '(condition-case err (progn (load "~/config/emacs/init.el") (kill-emacs 0)) (error (kill-emacs 1)))'


if [ $? -eq 0 ]; then
    echo "No errors"
else
    echo "There are errors"
fi