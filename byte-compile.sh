#!/usr/bin/env bash

set -e

EMACS_HOME="${HOME}/.emacs.d"
EMACS23="/Applications/Emacs.app/Contents/MacOS/"
EMACS=${EMACS23}Emacs

if [ -d "${EMACS23}" ]; then
    export PATH="${EMACS23}:${PATH}"
fi

echo "MAGIT"
cd ${EMACS_HOME}/magit
if [ -f "Makefile" ]; then
    make clean
fi
./autogen.sh
./configure
make

echo "ORG-MODE"
cd ${EMACS_HOME}/org-mode
make cleanall
make all

echo "SWANK-CLOJURE"
cd ${EMACS_HOME}/submodules/swank-clojure/
mvn clean
mvn install
cp target/swank-clojure-1.0-SNAPSHOT.jar ${HOME}/Development/java/jars-dev/swank-clojure.jar

cd ${EMACS_HOME}
