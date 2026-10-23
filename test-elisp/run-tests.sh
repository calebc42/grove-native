#!/bin/sh
set -eu

bloom_root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
repositories_root=${JETPACS_REPOSITORIES_ROOT:-"$(CDPATH= cd -- "$bloom_root/.." && pwd)"}
jetpacs_root=${JETPACS_ROOT:-"$repositories_root/jetpacs-poc"}
ebp_el_root=${EBP_EL_ROOT:-"$repositories_root/ebp.el"}
ebp_org_root=${EBP_ORG_ROOT:-"$repositories_root/ebp-org"}
jetpacs_components_root=${JETPACS_COMPONENTS_ROOT:-"$jetpacs_root/jetpacs-components"}

emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$bloom_root/elisp" \
  -l "$bloom_root/test-elisp/bloom-test.el" \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$bloom_root/elisp" \
  --eval "(progn (require 'checkdoc) (dolist (file (directory-files \"$bloom_root/elisp\" t \"\\\\.el\\\\'\")) (checkdoc-file file)))"

compile_output=$(mktemp -d /tmp/bloom-elisp-compile.XXXXXX)
trap 'rm -rf "$compile_output"' EXIT HUP INT TERM
BLOOM_COMPILE_OUTPUT="$compile_output" \
emacs -Q --batch \
  --eval '(setq load-prefer-newer t)' \
  -L "$ebp_el_root/lisp" \
  -L "$ebp_org_root/lisp" \
  -L "$jetpacs_root/emacs" \
  -L "$jetpacs_components_root/lisp/jetpacs-components" \
  -L "$bloom_root/elisp" \
  --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") (getenv \"BLOOM_COMPILE_OUTPUT\"))))" \
  -f batch-byte-compile \
  "$bloom_root/elisp/bloom-core.el" \
  "$bloom_root/elisp/bloom-design.el" \
  "$bloom_root/elisp/bloom-library.el" \
  "$bloom_root/elisp/bloom-org.el" \
  "$bloom_root/elisp/bloom-ui.el" \
  "$bloom_root/elisp/bloom-read.el" \
  "$bloom_root/elisp/bloom-document.el" \
  "$bloom_root/elisp/bloom-reminders.el" \
  "$bloom_root/elisp/bloom-structure.el" \
  "$bloom_root/elisp/bloom-views.el" \
  "$bloom_root/elisp/bloom-navigation.el" \
  "$bloom_root/elisp/bloom-widgets.el" \
  "$bloom_root/elisp/bloom.el"

test "$(find "$compile_output" -maxdepth 1 -name '*.elc' | wc -l)" -eq 13
