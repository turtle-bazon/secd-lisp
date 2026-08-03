#!/bin/bash
# compile-examples.sh — Compile secd-lisp examples to bytecode and C headers
#
# Copyright (C) 2026
# License: GPL3

set -e

SECD_LISP_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES_DIR="$SECD_LISP_DIR/examples"
OUTPUT_DIR="$SECD_LISP_DIR/build"
C_HEADER_DIR="$OUTPUT_DIR/headers"

# Create output directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$C_HEADER_DIR"

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found"
    echo "Install with: sudo apt install sbcl"
    exit 1
fi

# Check if xxd is available
if ! command -v xxd &> /dev/null; then
    echo "Error: xxd not found"
    echo "Install with: sudo apt install xxd"
    exit 1
fi

echo "Compiling secd-lisp examples..."
echo "=============================="

# Compile each example
for example in "$EXAMPLES_DIR"/*.lisp; do
    if [ -f "$example" ]; then
        filename=$(basename "$example" .lisp)
        output="$OUTPUT_DIR/$filename.secd"
        header="$C_HEADER_DIR/$filename.h"
        
        echo -n "Compiling $filename.lisp... "
        
        sbcl --non-interactive \
             --load "$SECD_LISP_DIR/secd-lisp.asd" \
             --eval '(asdf:load-system :secd-lisp)' \
             --eval "(let ((bytecode (secd-lisp:compile-file \"$example\"))) \
                       (secd-lisp:write-bytecode bytecode \"$output\") \
                       (format t \"OK (~A bytes)~%\" (length bytecode)))"
        
        if [ -f "$output" ]; then
            echo "  → $output"
            
            # Generate C header
            "$SECD_LISP_DIR/secd-to-c.sh" "$output" "$header"
        fi
    fi
done

echo ""
echo "Compiled files:"
echo "Bytecode (.secd):"
ls -la "$OUTPUT_DIR"/*.secd 2>/dev/null || echo "  No files"
echo ""
echo "C Headers:"
ls -la "$C_HEADER_DIR"/*.h 2>/dev/null || echo "  No files"

echo ""
echo "To use in secd-machine:"
echo "  #include \"headers/rp2040_blink.h\""
echo "  secd_execute(&machine, bytecode_rp2040_blink, bytecode_rp2040_blink_size);"
