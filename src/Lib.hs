{-# LANGUAGE MultilineStrings #-}
module Lib where

import GHC.Wasm.Prim

import Data.String (IsString(..))

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
    consoleLogVal :: JSVal -> IO ()

consoleLogShow :: Show a => a -> IO ()
consoleLogShow = consoleLogVal . stringAsVal . toJSString . show

foreign import javascript safe "const texture = await Assets.load($1); return texture"
    loadAsset :: JSString -> IO JSVal

foreign import javascript unsafe "new Sprite($1)"
    newSprite :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.anchor.set($2)"
    setAnchor :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.stage.addChild($2)"
    addChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1($2)"
  callFunction :: JSFunction -> JSVal -> IO JSVal

foreign import javascript unsafe "$1.ticker.add($2)"
    addTicker :: JSVal -> JSFunction -> IO ()

foreign import javascript "wrapper"
  jsFuncFromHs :: (JSVal -> IO JSVal) -> IO JSVal

foreign import javascript "$2[$1]"
  getProperty :: JSString -> JSVal -> IO JSVal

foreign import javascript "$2[$1] = $3"
  setProperty ::  JSString -> JSVal -> JSVal -> IO ()

foreign import javascript "$1"
  valAsFloat :: JSVal -> Float

foreign import javascript "$1"
  valAsInt :: JSVal -> Int

foreign import javascript "$1"
  valAsBool :: JSVal -> Bool

foreign import javascript "$1"
  valAsString :: JSVal -> JSString

foreign import javascript "$1"
  floatAsVal :: Float -> JSVal

foreign import javascript "$1"
  intAsVal :: Int -> JSVal

foreign import javascript "$1"
  boolAsVal :: Bool -> JSVal

foreign import javascript "$1"
  stringAsVal :: JSString -> JSVal

foreign import javascript "$2[$1] += $3"
  incrementProperty :: JSString -> JSVal -> JSVal -> IO ()

instance IsString JSString where
    fromString = toJSString