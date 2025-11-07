{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))

-- -- Export the actual initialization function
foreign export javascript "main" main :: IO ()


rotateSprite :: JSVal -> JSVal -> IO JSVal
rotateSprite sprite time = do
    dt <- valAsFloat <$> getProperty "deltaTime" time
    incrementProperty "rotation" sprite (floatAsVal 0.01)
    return sprite

main :: IO ()
main = do
    app <- newApp
    app <- initApp app $ toJSString "black"
    appendCanvas app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    texture <- loadAsset $ toJSString "assets/bunny.png"
    sprite <- newSprite texture
    consoleLogShow screen_width
    consoleLogShow screen_height
    setAnchor sprite 0.5
    setProperty "x" sprite (floatAsVal (fromIntegral screen_width / 2.0))
    setProperty "y" sprite (floatAsVal (fromIntegral screen_height / 2.0))
    addChild app sprite
    func <- jsFuncFromHs (rotateSprite sprite)
    addTicker app func
    return ()