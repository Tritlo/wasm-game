{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib
import GHC.Wasm.Prim

-- Dummy main function required by GHC, but we'll use -no-hs-main flag
main :: IO ()
main = error "not necessary"

-- Export the actual initialization function
foreign export javascript "main" initMain :: IO ()

initMain :: IO ()
initMain = do
    app <- newApp
    consoleLog app
    app <- initApp app $ toJSString "black"
    consoleLog app
    appendCanvas app
    screen_width <- getScreenWidth app
    screen_height <- getScreenHeight app
    loadAsset $ toJSString "assets/images/logo.png"
    sprite <- newSprite app
    setAnchor sprite 0.5
    setX sprite (fromIntegral screen_width / 2.0)
    setY sprite (fromIntegral screen_height / 2.0)
    addChild app sprite
    return ()