{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

-- | Rotate the sprite by the delta time.
rotateSprite :: JSVal -> JSVal -> IO JSVal
rotateSprite sprite time = do
    dt <- valAsFloat <$> getProperty "deltaTime" time
    incrementProperty "rotation" sprite (floatAsVal $ dt * 0.01)
    return sprite

main :: IO ()
main = do
    app <- newApp
    app <- initAppInTarget app "black" "#canvas-container"
    appendToTarget "#canvas-container" app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    sprite <- loadAsset "https://pixijs.com/assets/bunny.png" >>= newSprite
    setAnchor sprite 0.5
    setProperty "x" sprite (floatAsVal (fromIntegral screen_width / 2.0))
    setProperty "y" sprite (floatAsVal (fromIntegral screen_height / 2.0))
    addChild app sprite
    addTicker app =<< jsFuncFromHs (rotateSprite sprite)
    return ()