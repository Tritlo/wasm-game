#!/bin/bash
# Build script for Electron app
# This script builds the WASM files, vendors dependencies, and builds Electron app using Docker

set -e

echo "=== Building Electron App ==="

# Step 1: Build WASM files (if not already built)
if [ ! -f "test.wasm" ] || [ ! -f "ghc_wasm_jsffi.js" ]; then
    echo "Building WASM files..."
    ./build-and-copy.sh
else
    echo "WASM files already exist, skipping build"
fi

# Step 2: Vendor CDN dependencies
if [ ! -f "vendor/pixi.js@8.0.0/dist/pixi.min.js" ] || [ ! -f "vendor/@runno/wasi@0.7.0/dist/wasi.js" ]; then
    echo "Vendoring CDN dependencies..."
    ./scripts/vendor-dependencies.sh
else
    echo "Vendored dependencies already exist, skipping download"
fi

# Step 3: Create dist directory if it doesn't exist
mkdir -p dist

# Step 4: Build Electron app using Docker
echo ""
echo "=== Building Electron app in Docker ==="
echo "Building Docker image..."
docker build -f Dockerfile.electron -t ghc-wasm-pong-electron .

echo ""
echo "Running Electron build in Docker container..."
docker run --rm -v "$(pwd)/dist:/app/dist" ghc-wasm-pong-electron

echo ""
echo "=== Build Complete ==="
echo "Built packages are available in the dist/ directory:"
ls -lh dist/ 2>/dev/null || echo "  (dist directory is empty or doesn't exist)"
