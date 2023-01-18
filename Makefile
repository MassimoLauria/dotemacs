# Copyright (C) 2015, 2016, 2018, 2019, 2020, 2021, 2022 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2022-04-05, 23:27 (CEST) Massimo Lauria"
#

## Emacs binary

EMACS=emacs
EMACSCLIENT=emacsclient
EMACSAPP=Emacs.app

ifeq ($(shell uname -s),Darwin)
EMACS=/Applications/${EMACSAPP}/Contents/MacOS/Emacs
EMACSCLIENT=/Applications/${EMACSAPP}/Contents/MacOS/bin/emacsclient
endif

## Init files

INIT=$(abspath init.el)
INITFULL=$(abspath init-start.el)
INITMINI=$(abspath init-minimal.el)


FONTPATH=~/.fonts
ifeq ($(shell uname -s),Darwin)
FONTPATH=~/Library/Fonts
endif


.PHONY: clean test profile minisetup start stop install-fonts

# --------- Setup Emacs ---------------------------------
all:
	@echo "Setup Emacs editor (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@ln -s ${INIT} ~/.emacs.d
	${MAKE} install-pkgs
	@echo "Done."

minisetup:
	@echo "Minimal setup of Emacs (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@cp ${INITMINI} ~/.emacs.d/init.el
	@echo "Done."

emacs-changed:
	@echo "Setup a new emacs while preserving the current configuration."
	${MAKE} install-pkgs


uninstall:
	@echo "Completely erase Emacs configuration."
	@rm -fr ~/.emacs.d/init.el


# --------- Install tools, fonts and packages -----------
install-pkgs:
	@echo "Install the required emacs packages"
	${EMACS} -batch -l bootstrap.el -f install-pkgs
	${EMACS} -batch -l bootstrap.el --eval '(pdf-tools-install t)'


upgrade-pkgs:
	@echo "Update emacs packages"
	${EMACS} -batch -l bootstrap.el -f upgrade-pkgs
	${EMACS} -batch -l bootstrap.el --eval '(pdf-tools-install t)'


install-fonts:
	@echo "Install fonts for my configurations: "
	@rm -fr ${FONTPATH}/emacs
	@mkdir -p ${FONTPATH}/emacs
	unzip fonts/fira-code-5.2.zip        -d ${FONTPATH}/emacs
	unzip fonts/dejavu-fonts-2.37.zip   -d  ${FONTPATH}/emacs
	unzip fonts/all-the-icons-fonts.zip -d  ${FONTPATH}/emacs
	cp ./fonts/NotoColorEmoji.ttf           ${FONTPATH}/emacs
	-fc-cache -f -v

# --------- Measure setup quality -----------------------
test:
	@echo "Test whether the configuration loads correctly."
	@${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
	&& (echo "No error on load"; exit 0) \
	|| (echo "Test failed: errors on load. $$?"; exit 1)

# -------- Daemon ---------------------------------
start:
	${EMACS} --daemon --chdir ${HOME}

stop:
	${EMACSCLIENT} -e '(kill-emacs)'


# --------- Default rules -------------------------
clean:
	@-rm -f  *.elc
	@-rm -f  3rdparties/*.elc



# Local Variables:
# fill-column: 80
# End:
