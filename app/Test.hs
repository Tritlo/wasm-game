{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib
import GHC.Wasm.Prim


-- -- Export the actual initialization function
foreign export javascript "main" main :: IO ()

main :: IO ()
main = do
    app <- newApp
    app <- initApp app $ toJSString "black"
    appendCanvas app
    screen_width <- getScreenWidth app
    screen_height <- getScreenHeight app
    texture <- loadAsset $ toJSString "assets/bunny.png"
    sprite <- newSprite texture
    consoleLogShow screen_width
    consoleLogShow screen_height
    setAnchor sprite 0.5
    setX sprite (fromIntegral screen_width / 2.0)
    setY sprite (fromIntegral screen_height / 2.0)
    addChild app sprite
    return ()