#!/usr/bin/env bash

set -e

EMACS_HOME="${HOME}/.emacs.d"
EMACS23="/Applications/Emacs.app/Contents/MacOS/"
EMACS=${EMACS23}Emacs

if [ -d "${EMACS23}" ]; then
    export PATH="${EMACS23}:${PATH}"
fi

#echo "SLIME"
#SLIME_HOME="$EMACS_HOME/submodules/slime"
#gfind $SLIME_HOME -name '*.el' | gxargs $EMACS -batch -q -no-site-file -eval "(progn (add-to-list (quote load-path) \"$SLIME_HOME\") (add-to-list (quote load-path) (expand-file-name \"$SLIME_HOME/contrib\")))" -f batch-byte-compile

echo "SWANK-CLOJURE"
cd ${EMACS_HOME}/submodules/swank-clojure/
mvn clean
mvn install
cp target/swank-clojure-1.0-SNAPSHOT.jar ${HOME}/Development/java/jars-dev/swank-clojure.jar

cd ${EMACS_HOME}

