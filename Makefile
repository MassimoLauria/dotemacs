# Copyright (C) 2015, 2016, 2018 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2018-05-31, 10:16 (CEST) Massimo Lauria"
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


## Font files

FONTPATH=~/.fonts
ifeq ($(shell uname -s),Darwin)
FONTPATH=~/Library/Fonts
endif

FONTNAME=dejavu-fonts-ttf-2.37
FONTURL=http://sourceforge.net/projects/dejavu/files/dejavu/2.37/${FONTNAME}.zip


.PHONY: clean test profile minisetup start stop install-fonts

# --------- Setup Emacs ---------------------------------
all: ${CASKBIN}
	@echo "Setup Emacs editor (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@rm -f ~/.emacs.d/Cask
	@ln -s ${INIT} ~/.emacs.d
	@ln -s ${CASK} ~/.emacs.d
	${MAKE} install-pkgs
	${MAKE}	install-fonts
	@echo "Done."

minisetup:
	@echo "Minimal setup of Emacs (override any previous configuration)."
	@mkdir -p ~/.emacs.d
	@rm -f ~/.emacs.d/init.el
	@cp ${INITMINI} ~/.emacs.d/init.el
	@echo "Done."


uninstall:
	@echo "Completely erase Emacs configuration."
	@rm -fr ~/.emacs.d/init.el
	@rm -fr ~/.emacs.d/Cask
	@rm -fr ~/.emacs.d/.cask/
	@rm -fr ~/.emacs.d/anaconda-mode/
	@rm -fr ~/.emacs.d/irony/
	@rm -fr ~/.cask/


# --------- Install tools, fonts and packages -----------
install-pkgs: ${CASKBIN}
	@echo "Install the required packages."
	$(shell EMACS=${EMACS} ${CASKBIN} install --verbose --path ${HOME}/.emacs.d)


install-fonts:
	@echo "Install DejaVu Sans Mono fonts"
	@-curl -fSL ${FONTURL} -o ${FONTNAME}.zip
	@unzip -q ${FONTNAME}.zip
	@mkdir -p ${FONTPATH}
	@mv ${FONTNAME}/ttf/*.ttf ${FONTPATH}
	@rm -fr ${FONTNAME} ${FONTNAME}.zip

${CASKBIN}:
	@-curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python


# --------- Measure setup quality -----------------------
profile:
	@echo "Profile the init file."
	${EMACS} -Q -l utils/profile-dotemacs.el \
	--eval '(setq profile-dotemacs-file (setq load-file-name "${INITFULL}"))' \
    -f profile-dotemacs


test:
	@echo "Test whether the configuration loads correctly."
	@${EMACS} -q --eval '(condition-case err (progn (load "${INIT}") (kill-emacs 0)) (error (kill-emacs 1)))' \
	&& (echo "No error on load"; exit 0) \
	|| (echo "Test failed: errors on load. $$?"; exit 1)

# -------- Daemon ---------------------------------
startdaemon:
	${EMACS} --daemon --chdir ${HOME}

stopdaemon:
	${EMACSCLIENT} -e '(kill-emacs)'



# --------- Default rules -------------------------
clean:
	@-rm -f  *.elc
	@-rm -f  3rdparties/*.elc
	@-rm -f  utils/*.elc



# Local Variables:
# fill-column: 80
# End:
