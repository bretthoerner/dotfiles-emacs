# Name of your emacs binary
EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs

# Where local software is found
EMACS_HOME = $(HOME)/.emacs.d

# FIXME: Add all directories
# Where local lisp files go.
lispdir = $(EMACS_HOME)/misc

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(progn (add-to-list (quote load-path) \"$(lispdir)\") \
 	  (add-to-list (quote load-path) (expand-file-name \"./lisp/\")))"

# Specify the byte-compiler for compiling org-mode files
ELC = $(BATCH) -f batch-byte-compile

# The following variables need to be defined by the maintainer
MISCF = ack.el \
		browse-kill-ring.el \
		dired-single.el \
		doctest-mode.el \
		erlang-start.el \
		erlang.el \
		find-file-in-project.el \
		flymake-cursor.el \
		goto-last-change.el \
		haml-mode.el \
		idle-highlight.el \
		js2-mode.el \
		mac-key-mode.el \
		markdown-mode.el \
		paredit.el \
		php-mode.el \
		project.el \
		python-mode.el \
		redo.el \
		revbufs.el \
		sass-mode.el

OTHERF = color-theme/color-theme.el

LISPFILES  = $(MISCF:%=misc/%) $(OTHERF)
ELCFILES   = $(LISPFILES:.el=.elc)

.SUFFIXES: .el .elc
SHELL = /bin/sh

default: $(ELCFILES)

all: $(ELCFILES)

magit: magit/magit.elc

magit/magit.elc:
	magit/autogen.sh
	magit/configure
	make clean all

update:
	git submodule foreach "git pull"
	${MAKE} clean
	${MAKE} all

compile: $(ELCFILES)

cleanelc:
	rm -f $(ELCFILES)

clean:
	${MAKE} cleanelc
	# FIXME: Clean other projects?

cleanall:
	${MAKE} clean

.el.elc:
	$(ELC) $<
