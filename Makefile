# Copyright (C) 2015, 2016, 2018, 2019, 2020 by Massimo Lauria <lauria.massimo@gmail.com>
#
# Created   : "2015-05-10, Sunday 19:08 (CEST) Massimo Lauria"
# Time-stamp: "2020-03-17, 13:04 (CET) Massimo Lauria"
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

## Font files

FONTS=dejavu-fonts-ttf-2.37 iosevska-fonts-ttf-2.1.0 baskerville-font source-code-pro

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
	${MAKE}	install-fonts
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
	@rm -fr ~/.emacs.d/anaconda-mode/
	@rm -fr ~/.emacs.d/irony/


# --------- Install tools, fonts and packages -----------
install-pkgs:
	@echo "Install the required emacs packages"
	${EMACS} -batch -l bootstrap.el -f install-pkgs

upgrade-pkgs:
	@echo "Update emacs packages"
	${EMACS} -batch -l bootstrap.el -f upgrade-pkgs


install-fonts:
	@echo "Install fonts: ${FONTS}"
	@mkdir -p ${FONTPATH}
	@for font in ${FONTS}                       ;\
	     do unzip fonts/$$font.zip -d fonts/    ;\
             mv fonts/$$font/ttf/*.ttf ${FONTPATH}  ;\
	     rm -fr fonts/$$font/                   ;\
	done

# --------- Measure setup quality -----------------------
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



# Local Variables:
# fill-column: 80
# End:
