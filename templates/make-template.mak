# Copyright (C) 2010 by >>>NAME<<< <>>>EMAIL<<<>
#
# Created   : ">>>TIME<<< >>>NAME<<<"
# Time-stamp: " "

# ---------- Environment variables ------------
#
CC=gcc
#CC=cc

C_STANDARD=-std=c99
#C_STANDARD=-ansi
#C_STANDARD=-std=c89

TAGS=gtags
#TAGS=etags
#TAGS=ctags
TAGFILES=GPATH GRTAGS GSYMS GTAGS tags TAGS ID

CFLAGS=-fno-builtin --pedantic --pedantic-errors -Wall ${C_STANDARD}
LDFLAGS=

DEBUG=-DDEBUG -g
#DEBUG=

# --------- Project dependent rules ---------------
TARGET=>>>POINT<<<
all: tags ${TARGET}

# <Insert custom rules here>





# --------- Default rules -------------------------
clean:
	@-rm -f ${TARGET}
	@-rm -f *.o
	@-rm -fr *.dSYM
	@-rm -f ${TAGFILES}

tags:
	@-$(TAGS) .

check-syntax:
	$(CC) ${CFLAGS} -o - -S ${CHK_SOURCES} >/dev/null

%.o: %.c
	$(CC) ${CFLAGS} -c $< -o $@


# Local Variables:
# fill-column: 80
# End:
