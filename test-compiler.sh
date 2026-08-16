#!/bin/bash
# test-compiler.sh — Test secd-lisp compiler on PC
#
# Copyright (C) 2026
# License: GPL3

set -e

SECD_LISP_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Testing secd-lisp compiler..."
echo "============================="

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found"
    exit 1
fi

# Test compilation
sbcl --non-interactive \
     --load "$SECD_LISP_DIR/secd-lisp.asd" \
     --eval '(asdf:load-system :secd-lisp)' \
     --eval '(format t "Lexer test: ~A~%" (length (secd-lisp:tokenize "(+ 1 2)")))' \
     --eval '(format t "Parser test: ~A~%" (secd-lisp:ast-node-type (secd-lisp:parse (secd-lisp:tokenize "(+ 1 2)"))))' \
     --eval '(format t "Compiler test: ~A bytes~%" (length (secd-lisp:compile-string "(defun main () (+ 1 2))")))'

echo ""
echo "All tests passed!"
