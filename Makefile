# Name of your emacs binary
EMACS = /usr/bin/emacs

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
MISCF =	browse-kill-ring.el \
		buffer-move.el \
		dired-single.el \
		erc-libnotify.el \
		find-file-in-project.el \
		flymake-cursor.el \
		full-ack.el \
		goto-last-change.el \
		highlight-parentheses.el \
		http-twiddle.el \
		idle-highlight.el \
		js2-mode.el \
		js-comint.el \
		mac-key-mode.el \
		markdown-mode.el \
		multi-term.el \
		paredit.el \
		php-mode.el \
		project.el \
		puppet-mode.el \
		quack.el \
		rainbow-mode.el \
		redo.el \
		scpaste.el \
		scratch.el \
		smooth-scrolling.el

LISPFILES  = $(MISCF:%=misc/%)
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

