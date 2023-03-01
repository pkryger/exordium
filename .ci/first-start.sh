#!/bin/bash

set -x
set -e

EMACS_DIR="$(cd "${GITHUB_WORKSPACE:-~}/${1:-.emacs.d}"; pwd -P)/"
EMACS="${EMACS:=emacs}"

# Installing packages from melpa seems to stumble on ask-user-about-lock.  This
# manifests with signal(error ("Cannot resolve lock conflict in batch mode")).
# Preventing creating them with setting create-lockfiles to nil.
${EMACS} -Q --batch \
         --eval '
(progn
   (setq debug-on-error t
         user-emacs-directory "'"${EMACS_DIR}"'"
         create-lockfiles nil)
   (load-file "'"${EMACS_DIR}"'/init.el"))'
