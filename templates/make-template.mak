# Copyright (C) 2010 by >>>NAME<<< <>>>EMAIL<<<>
#
# Created   : ">>>TIME<<< >>>NAME<<<"
# Time-stamp: "2010-10-25, lunedì 11.25 (CEST) Massimo Lauria"

# Description::
#
# >>>POINT<<<
#

# ------ TARGETS -------------------
TARGET=


# ------ COMPILER SETTINGS ---------

# Define the C compiler and standard to use for the exercises
CC=gcc
C_STANDARD=-std=c89 -ansi
#C_STANDARD=-std=c99 -ansi


CFLAGS=-g -fno-builtin -pedantic -Wall ${C_STANDARD}


# ------ DEFAULT RULES --------------
all: ${TARGET}

clean:
	rm ${TARGET}

%.o: %.c
	$(CC) ${CFLAGS} -c $< -o $@

# Rule for flymake emacs
check-syntax:
	$(CC) -o - -S ${CHK_SOURCES}  >/dev/null


# ------ CUSTOM RULES ---------------
#
# <Put here any build rule which is not custom>
#

# Local Variables:
# fill-column: 80
# End:
