# GHC WASM Game

This demo showcases running Haskell code compiled to WebAssembly using the [GHC WASM backend](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).
The GHC WASM backend allows you to compile Haskell programs to WebAssembly, enabling functional
programming in the browser with full access to JavaScript APIs through FFI, in this case to [pixi.js](https://pixijs.com/).

## Steam Deck support

Check out the electron branch to see how we can run it on Steam Deck!


<img width="3533" height="1987" alt="image" src="https://github.com/user-attachments/assets/d665fe25-e8b4-471f-acad-9733d8ef0325" />


## Running

To build and run the project:

1. Build the Docker image and copy the compiled files:
   ```bash
   ./build-and-copy.sh
   ```

2. Start a local web server:
   ```bash
   python -m http.server 8001
   ```

3. Open your browser and navigate to:
   ```
   http://localhost:8001
   ```
