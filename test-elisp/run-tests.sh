#!/bin/sh
set -eu

grove_root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
jetpacs_root=${JETPACS_ROOT:-"$grove_root/../jetpacs/jetpacs-poc/jetpacs"}
ebp_el_root=${EBP_EL_ROOT:-"$grove_root/../jetpacs/jetpacs-poc/ebp-poc/ebp.el"}
ebp_org_root=${EBP_ORG_ROOT:-"$grove_root/../jetpacs/jetpacs-poc/ebp-poc/ebp-org"}
jetpacs_components_root=${JETPACS_COMPONENTS_ROOT:-"$grove_root/../jetpacs/jetpacs-poc/jetpacs-components"}

emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$grove_root/elisp" \
  -l "$grove_root/test-elisp/grove-test.el" \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$grove_root/elisp" \
  --eval "(progn (require 'checkdoc) (dolist (file (directory-files \"$grove_root/elisp\" t \"\\\\.el\\\\'\")) (checkdoc-file file)))"

compile_output=$(mktemp -d /tmp/grove-elisp-compile.XXXXXX)
trap 'rm -rf "$compile_output"' EXIT HUP INT TERM
GROVE_COMPILE_OUTPUT="$compile_output" \
emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$grove_root/elisp" \
  --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") (getenv \"GROVE_COMPILE_OUTPUT\"))))" \
  -f batch-byte-compile \
  "$grove_root/elisp/grove-core.el" \
  "$grove_root/elisp/grove-design.el" \
  "$grove_root/elisp/grove-library.el" \
  "$grove_root/elisp/grove-org.el" \
  "$grove_root/elisp/grove-ui.el" \
  "$grove_root/elisp/grove-document.el" \
  "$grove_root/elisp/grove-reminders.el" \
  "$grove_root/elisp/grove-structure.el" \
  "$grove_root/elisp/grove-views.el" \
  "$grove_root/elisp/grove-navigation.el" \
  "$grove_root/elisp/grove-widgets.el" \
  "$grove_root/elisp/grove.el"

test "$(find "$compile_output" -maxdepth 1 -name '*.elc' | wc -l)" -eq 12
