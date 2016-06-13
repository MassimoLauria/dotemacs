# Copyright (C) 2015, 2016 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2016-06-13, 11:46 (CEST) Massimo Lauria"
#

## Emacs binary

EMACS=emacs
EMACSCLIENT=emacsclient

ifeq ($(shell uname -s),Darwin)
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
endif

## Init files

INIT=$(abspath init.el)
INITFULL=$(abspath init-start.el)
INITMINI=$(abspath init-minimal.el)

## Cask files

CASK=$(abspath Cask)
CASKBIN=${HOME}/.cask/bin/cask



.PHONY: clean test profile minisetup start stop

all: ${CASKBIN}
	@echo "Setup Emacs editor (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@rm -f ~/.emacs.d/Cask
	@ln -s ${INIT} ~/.emacs.d
	@ln -s ${CASK} ~/.emacs.d
	@echo "Install required packages."
	${CASKBIN} install --path ${HOME}/.emacs.d
	@echo "Done."

minisetup:
	@echo "Minimal setup of Emacs (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@cp ${INITMINI} ~/.emacs.d/init.el

profile:
	${EMACS} -Q -l utils/profile-dotemacs.el \
	--eval '(setq profile-dotemacs-file (setq load-file-name "${INITFULL}"))' \
    -f profile-dotemacs


test:
	@echo "Run emacs to test the configuration."
	@${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
	&& (echo "No error on load"; exit 0) \
	|| (echo "Test failed: errors on load. $$?"; exit 1)


# --------- Setup ---------------------------------
${CASKBIN}:
	@-curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python



uninstall:
	@rm -fr ~/.emacs.d/init.el
	@rm -fr ~/.emacs.d/Cask
	@rm -fr ~/.emacs.d/.cask/
	@rm -fr ~/.emacs.d/anaconda-mode/
	@rm -fr ~/.emacs.d/irony/
	@rm -fr ~/.cask/

# -------- Daemon ---------------------------------
start:
	${EMACS} --daemon --chdir ${HOME}

stop:
	${EMACSCLIENT} -e '(kill-emacs)'



# --------- Default rules -------------------------
clean:
	@-rm -f  *.elc
	@-rm -f  3rdparties/*.elc
	@-rm -f  utils/*.elc



# Local Variables:
# fill-column: 80
# End:
