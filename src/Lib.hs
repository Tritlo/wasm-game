module Lib where

import GHC.Wasm.Prim

succ :: Int -> Int
succ x = x + 1

type JSFunction = JSVal

foreign import javascript unsafe "new Application()"
   newApp :: IO JSVal

foreign import javascript unsafe "(s => {s.init({background: $2, resizeTo: window}); return s})($1)"
    initApp :: JSVal -> JSString -> IO JSVal

foreign import javascript unsafe "(s => document.body.appendChild(s.canvas))($1)"
    appendCanvas :: JSVal -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLog :: JSVal -> IO ()

foreign import javascript unsafe "Assets.load($1)"
    loadAsset :: JSString -> IO ()

foreign import javascript unsafe "new Sprite($1)"
    newSprite :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.anchor.set($2)"
    setAnchor :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.x = $2"
    setX :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.y = $2"
    setY :: JSVal -> Float -> IO ()

foreign import javascript unsafe
  "(s =>{ console.log('app'); console.log(s); console.log(s.screen); return s.screen.width})($1)"
    getScreenWidth :: JSVal -> IO Int


foreign import javascript unsafe "$1.screen.height"
    getScreenHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.stage.addChild($2)"
    addChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.ticker.add($2)"
    addTicker :: JSVal -> JSFunction -> IO ()