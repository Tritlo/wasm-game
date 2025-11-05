#!/bin/bash
# Script to generate ghc_wasm_jsffi.js from the compiled wasm file
# Usage: ./generate-jsffi.sh [path-to-wasm-file]

WASM_FILE="${1:-dist-newstyle/build/wasm32-wasi/ghc-*/test/x/test/build/test/test.wasm}"
OUTPUT_FILE="ghc_wasm_jsffi.js"

# Find the wasm file if using default path
if [ ! -f "$WASM_FILE" ]; then
    # Try to find it in dist-newstyle
    WASM_FILE=$(find dist-newstyle -name "test.wasm" -type f | head -n 1)
fi

if [ ! -f "$WASM_FILE" ]; then
    echo "Error: Could not find test.wasm file"
    echo "Please provide the path to the wasm file as an argument"
    exit 1
fi

# Check if wasm32-wasi-ghc is available
if ! command -v wasm32-wasi-ghc &> /dev/null; then
    echo "Error: wasm32-wasi-ghc not found in PATH"
    echo "Please ensure the GHC wasm cross compiler is installed and in your PATH"
    exit 1
fi

# Get the libdir and run post-link.mjs
LIBDIR=$(wasm32-wasi-ghc --print-libdir)
POST_LINK="$LIBDIR/post-link.mjs"

if [ ! -f "$POST_LINK" ]; then
    echo "Error: post-link.mjs not found at $POST_LINK"
    exit 1
fi

echo "Generating ghc_wasm_jsffi.js from $WASM_FILE..."
node "$POST_LINK" -i "$WASM_FILE" -o "$OUTPUT_FILE"

if [ $? -eq 0 ]; then
    echo "Successfully generated $OUTPUT_FILE"
else
    echo "Error: Failed to generate $OUTPUT_FILE"
    exit 1
fi
