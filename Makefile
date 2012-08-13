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
MISCF =	ac-slime.el \
		browse-kill-ring.el \
		buffer-move.el \
		c-eldoc.el \
		coffee-mode.el \
		deft.el \
		diminish.el \
		dired-single.el \
		find-file-in-project.el \
		flymake-cursor.el \
		full-ack.el \
		furl.el \
		go-mode.el \
		goto-last-change.el \
		highlight-parentheses.el \
		htmlize.el \
		http-twiddle.el \
		idle-highlight-mode.el \
		ido-ubiquitous.el \
		js2-mode.el \
		js-comint.el \
		mac-key-mode.el \
		markdown-mode.el \
		multi-term.el \
		nagios-mode.el \
		nitrogen-mode.el \
		paredit.el \
		php-mode.el \
		popup.el \
		puppet-mode.el \
		python.el \
		quack.el \
		rainbow-mode.el \
		rcirc-notify.el \
		redo+.el \
		scpaste.el \
		scratch.el \
		smex.el \
		smooth-scrolling.el \
                undo-tree.el \
		vcl-mode.el \
		xcscope.el

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

