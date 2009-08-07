#!/usr/bin/env bash

EMACS_HOME="${HOME}/.emacs.d"
EMACS23="/Applications/Emacs.app/Contents/MacOS/"
EMACS=${EMACS23}Emacs

if [ -d "${EMACS23}" ]; then
    export PATH="${EMACS23}:${PATH}"
fi

echo "MAGIT"
cd ${EMACS_HOME}/magit
./autogen.sh
./configure
make

echo "MISC"
for filename in erlang paredit python-mode
do
    emacs -batch -q -eval "(byte-compile-file \"${EMACS_HOME}/misc/${filename}.el\")"
done

echo "ORG-MODE"
cd ${EMACS_HOME}/org-mode
make cleanall
make

cd ${EMACS_HOME}
