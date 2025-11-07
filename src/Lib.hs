{-# LANGUAGE MultilineStrings #-}
module Lib where

import GHC.Wasm.Prim

succ :: Int -> Int
succ x = x + 1

type JSFunction = JSVal

foreign import javascript unsafe "new Application()"
   newApp :: IO JSVal

foreign import javascript safe
 """
  const r = await $1.init({background: $2, resizeTo: window});
  return $1
 """
 initApp :: JSVal -> JSString -> IO JSVal

foreign import javascript unsafe "document.body.appendChild($1.canvas)"
    appendCanvas :: JSVal -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLog :: JSVal -> IO ()
foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogString :: JSString -> IO ()

consoleLogShow :: Show a => a -> IO ()
consoleLogShow = consoleLogString . toJSString . show

foreign import javascript safe "const texture = await Assets.load($1); return texture"
    loadAsset :: JSString -> IO JSVal

foreign import javascript unsafe "new Sprite($1)"
    newSprite :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.anchor.set($2)"
    setAnchor :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.x = $2"
    setX :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.y = $2"
    setY :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.screen.width"
    getScreenWidth :: JSVal -> IO Int

foreign import javascript unsafe "$1.screen.height"
    getScreenHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.stage.addChild($2)"
    addChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.ticker.add($2)"
    addTicker :: JSVal -> JSFunction -> IO ()