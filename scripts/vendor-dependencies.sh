#!/bin/bash
# Script to download and vendor CDN dependencies using curl

set -e  # Exit on error

echo "Vendoring CDN dependencies..."

# Create vendor directory structure
mkdir -p vendor/pixi.js@8.0.0/dist
mkdir -p vendor/@runno/wasi@0.7.0/dist

# Download PIXI.js
echo "Downloading PIXI.js..."
curl -f -L https://cdn.jsdelivr.net/npm/pixi.js@8.0.0/dist/pixi.min.js -o vendor/pixi.js@8.0.0/dist/pixi.min.js

# Download @runno/wasi
echo "Downloading @runno/wasi..."
curl -f -L https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js -o vendor/@runno/wasi@0.7.0/dist/wasi.js

# Verify downloads
if [ ! -f "vendor/pixi.js@8.0.0/dist/pixi.min.js" ]; then
    echo "Error: Failed to download PIXI.js"
    exit 1
fi

if [ ! -f "vendor/@runno/wasi@0.7.0/dist/wasi.js" ]; then
    echo "Error: Failed to download @runno/wasi"
    exit 1
fi

echo "✓ Successfully vendored all dependencies"
ls -lh vendor/pixi.js@8.0.0/dist/pixi.min.js vendor/@runno/wasi@0.7.0/dist/wasi.js
