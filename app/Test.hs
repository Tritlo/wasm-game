{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when)

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

-- | Rotate the sprite by the delta time.
rotateSprite :: JSVal -> JSVal -> IO ()
rotateSprite sprite time = do
    dt <- valAsFloat <$> getProperty "deltaTime" time
    incrementProperty "rotation" sprite (floatAsVal $ dt * 0.01)

data State = State {
    mouseX :: Float,
    mouseY :: Float
}

moveSprite :: JSVal -> IORef State -> JSVal -> IO ()
moveSprite sprite state_ref _ = do
    state <- readIORef state_ref
    setProperty "x" sprite (floatAsVal $ state.mouseX)
    setProperty "y" sprite (floatAsVal $ state.mouseY)



main :: IO ()
main = do
    app <- newApp
    app <- initAppInTarget app "black" "#canvas-container"
    appendToTarget "#canvas-container" app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    -- Sprite
    sprite <- loadAsset "https://pixijs.com/assets/bunny.png" >>= newSprite
    setProperty "eventMode" sprite (stringAsVal "static")
    setAnchor sprite 0.5
    let initial_state = State { mouseX = fromIntegral screen_width / 2.0,
                               mouseY = fromIntegral screen_height / 2.0 }
    state_ref <- newIORef initial_state
    setProperty "x" sprite (floatAsVal $ initial_state.mouseX)
    setProperty "y" sprite (floatAsVal $ initial_state.mouseY)
    addChild app sprite
    fps_counter <- newText "0" "white"
    setProperty "x" fps_counter (floatAsVal 10.0)
    setProperty "y" fps_counter (floatAsVal 10.0)
    addChild app fps_counter
    addTicker app =<< jsFuncFromHs_ (rotateSprite sprite)
    addTicker app =<< jsFuncFromHs_ (moveSprite sprite state_ref)

    -- FPS counter
    fps_ticker <- newTicker
    setProperty "maxFPS" fps_ticker (intAsVal 10)
    startTicker fps_ticker
    callAddTicker fps_ticker =<< jsFuncFromHs_ (\_ -> do
            fps <- fmap valAsFloat $ getPropertyKey ["ticker", "FPS"] app
            let fps_val = floor fps
            setProperty "text" fps_counter (stringAsVal $ toJSString $ show fps_val)
        )

    paddle <- baseTexture "WHITE" >>= newSprite
    setProperty "eventMode" paddle (stringAsVal "static")
    setProperty "width" paddle (floatAsVal 50.0)
    setProperty "height" paddle (floatAsVal 10.0)
    setAnchor paddle 0.5
    setProperty "x" paddle (floatAsVal $ (fromIntegral  screen_width) / 2.0)
    setProperty "y" paddle (floatAsVal $ (fromIntegral  screen_height) - 100.0)
    addChild app paddle

    addEventListener "globalpointermove" paddle =<< jsFuncFromHs_
      (\event -> do
                    mx <- valAsFloat <$> getPropertyKey ["screen", "x"] event
                    when (mx >= 0.0 && mx <= fromIntegral screen_width) $ do
                        setProperty "x" paddle (floatAsVal mx)
                    )
