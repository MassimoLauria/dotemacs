# Copyright (C) 2015 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2015-06-24, 01:46 (CEST) Massimo Lauria"
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

all:
	@echo "Install config file."
	mkdir -p ~/.emacs.d
	rm ~/.emacs.d/init.el
	ln -s ${INIT} ~/.emacs.d
	@echo "First launch to test/install required packages."
	@${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
		&& (echo "Successful installation."; exit 0) \
		|| (echo "Error during first run. $$?"; exit 1)

profile:
	${EMACS} -Q -l utils/profile-dotemacs.el \
	--eval '(setq profile-dotemacs-file (setq load-file-name "${INITFULL}"))' \
    -f profile-dotemacs


test:
	@${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
	&& (echo "No error on load"; exit 0) \
	|| (echo "Test failed: errors on load. $$?"; exit 1)


# --------- Default rules -------------------------
clean:
	@-rm -f  *.elc
	@-rm -f  3rdparties/*.elc
	@-rm -f  utils/*.elc



# Local Variables:
# fill-column: 80
# End:
