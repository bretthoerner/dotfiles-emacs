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
MISCF = autopair.el \
		browse-kill-ring.el \
		dired-single.el \
		find-file-in-project.el \
		flymake-cursor.el \
		full-ack.el \
		goto-last-change.el \
		growl.el \
		hl-line+.el \
		http-twiddle.el \
		idle-highlight.el \
		js-comint.el \
		js2-mode.el \
		mac-key-mode.el \
		markdown-mode.el \
		paredit.el \
		project.el \
		redo.el \
		smooth-scrolling.el

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
	git submodule foreach "git pull upstream master; git push origin"
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

