# GHC WASM Pong

A classic Pong game implemented in Haskell and compiled to WebAssembly using the [GHC WASM backend](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta). The game runs in the browser or as a standalone Electron application with full gamepad support.

## Features

- Classic Pong gameplay with physics-based ball movement
- Player vs Computer AI
- Fullscreen mode for immersive gameplay
- **Gamepad support** - Control the paddle with a gamepad (left stick X axis)
- **Mouse control** - Move the paddle with your mouse
- Cross-platform Electron builds (Linux AppImage/Deb, Windows NSIS/Portable)
- All dependencies vendored for offline builds

## Running in Browser

To build and run the project in a browser:

1. Build the Docker image and copy the compiled files:
   ```bash
   ./build-and-copy.sh
   ```

2. Vendor the dependencies:
   ```bash
   ./scripts/vendor-dependencies.sh
   ```

3. Start a local web server:
   ```bash
   python -m http.server 8001
   ```

4. Open your browser and navigate to:
   ```
   http://localhost:8001
   ```

## Building Electron App

To build standalone Electron applications:

```bash
./build-electron.sh
```

This will:
1. Build the WASM files (if needed)
2. Vendor all CDN dependencies
3. Build Electron apps for Linux and Windows using Docker

Built packages will be available in the `dist/` directory:
- **Linux**: AppImage and Debian package (.deb)
- **Windows**: NSIS installer and portable executable

## Running Electron App Locally

To run the Electron app in development mode:

```bash
npm install
npm run electron:dev
```

## Controls

- **Mouse**: Move your mouse horizontally to control the bottom paddle
- **Gamepad**: Use the left stick X axis to control the bottom paddle
  - Gamepad input takes precedence when active
  - Deadzone applied to prevent drift

## Technical Details

- Built with GHC WASM backend for WebAssembly compilation
- Uses PIXI.js for 2D rendering
- WASI runtime for WebAssembly execution
- Electron for desktop packaging
- All JavaScript dependencies vendored for offline operation

## Project Structure

- `app/Test.hs` - Main game logic and Pong implementation
- `src/Lib.hs` - PIXI.js FFI bindings and gamepad API wrappers
- `electron/` - Electron main process and preload scripts
- `assets/` - Game assets (Haskell logo for ball sprite)
- `vendor/` - Vendored JavaScript dependencies
- `Dockerfile` - Build environment for WASM compilation
- `Dockerfile.electron` - Build environment for Electron packaging
