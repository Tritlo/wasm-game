#!/bin/bash
# Script to build the Docker image and copy the compiled wasm and jsffi files

set -e  # Exit on error

echo "Building Docker image..."
docker build -t wasm-t .

echo "Finding test.wasm in container..."
# Find the wasm file dynamically (handles different GHC versions)
WASM_PATH=$(docker run --rm wasm-t find dist-newstyle -name "test.wasm" -type f | head -n 1)

if [ -z "$WASM_PATH" ]; then
    echo "Error: Could not find test.wasm in container"
    exit 1
fi

echo "Found wasm file at: $WASM_PATH"
echo "Copying test.wasm from container..."
docker run -v $(pwd):/pwd --rm wasm-t cp "$WASM_PATH" /pwd/test.wasm

echo "Copying ghc_wasm_jsffi.js from container..."
docker run -v $(pwd):/pwd --rm wasm-t cp /app/ghc_wasm_jsffi.js /pwd

echo "Done! Files copied to current directory:"
ls -lh test.wasm ghc_wasm_jsffi.js 2>/dev/null || echo "Warning: Some files may not exist"
