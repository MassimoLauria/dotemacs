# Copyright (C) 2015 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2015-05-10, 19:42 (CEST) Massimo Lauria"
#




## Binary

EMACS=emacs

ifeq ($(shell uname -s),Darwin)
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
endif


## Init files

INIT=$(abspath init.el)
INITFULL=$(abspath init-start.el)




.PHONY: clean test profile

# all:
# 	${EMACS} -Q --eval "(byte-recompile-directory \"$(abspath .)\" 0 t)"

profile:
	${EMACS} -Q -l utils/profile-dotemacs.el \
	--eval '(setq profile-dotemacs-file (setq load-file-name "${INITFULL}"))' \
    -f profile-dotemacs


test:
	${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
	|| (echo "Test failed $$?"; exit 1)


# --------- Default rules -------------------------
clean:
	@-rm -f  *.elc
	@-rm -f  3rdparties/*.elc
	@-rm -f  utils/*.elc



# Local Variables:
# fill-column: 80
# End:
