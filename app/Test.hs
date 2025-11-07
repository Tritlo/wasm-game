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
    mouseY :: Float,
    spriteXSpeed :: Float,
    spriteYSpeed :: Float
}

-- | Move sprite downward with constant speed and respawn at center if it falls out
fallSprite :: IORef State -> Float -> Float -> JSVal -> JSVal -> JSVal -> IO ()
fallSprite state_ref screen_width screen_height sprite paddle ticker = do
    state <- readIORef state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker
    current_y <- valAsFloat <$> getProperty "y" sprite
    current_x <- valAsFloat <$> getProperty "x" sprite
    paddle_x <- valAsFloat <$> getProperty "x" paddle
    paddle_y <- valAsFloat <$> getProperty "y" paddle
    paddle_width <- valAsFloat <$> getProperty "width" paddle
    let new_y = current_y + state.spriteYSpeed * dt
    let new_x = current_x + state.spriteXSpeed * dt
        paddle_half_width = paddle_width / 2.0
        paddle_left = paddle_x - paddle_half_width
        paddle_right = paddle_x + paddle_half_width
        -- Check if sprite x is within paddle x ± half width
        x_collision = new_x >= paddle_left && new_x <= paddle_right
        -- Check if sprite is at paddle y level (with some tolerance)
        y_collision = abs (new_y - paddle_y) < 20.0 && state.spriteYSpeed > 0.0

    -- Check for paddle collision
    if x_collision && y_collision then
        -- Bounce off paddle: reverse y speed
        do
            let new_state = state { spriteYSpeed = -state.spriteYSpeed }
            writeIORef state_ref new_state
            setProperty "x" sprite (floatAsVal new_x)
            setProperty "y" sprite (floatAsVal new_y)
    else if new_y < 0.0 then do
            let new_state = state { spriteYSpeed = abs state.spriteYSpeed }
            writeIORef state_ref new_state
            setProperty "x" sprite (floatAsVal new_x)
            setProperty "y" sprite (floatAsVal 0.0)
    else if new_y > screen_height then do
        do let new_state = state { spriteYSpeed = abs state.spriteYSpeed }
           writeIORef state_ref new_state
           setProperty "x" sprite (floatAsVal $ screen_width / 2.0)
           setProperty "y" sprite (floatAsVal $ screen_height / 2.0)
    else if new_x < 0.0 then do
            let new_state = state { spriteXSpeed = abs state.spriteXSpeed }
            writeIORef state_ref new_state
            setProperty "x" sprite (floatAsVal 0.0)
            setProperty "y" sprite (floatAsVal new_y)
    else if new_x > screen_width then do
            let new_state = state { spriteXSpeed = abs state.spriteXSpeed }
            writeIORef state_ref new_state
            setProperty "x" sprite (floatAsVal screen_width)
            setProperty "y" sprite (floatAsVal new_y)
    else do
            setProperty "x" sprite (floatAsVal new_x)
            setProperty "y" sprite (floatAsVal new_y)



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
                               mouseY = fromIntegral screen_height / 2.0,
                               spriteXSpeed = 0.0,
                               spriteYSpeed = 5.0 }
    state_ref <- newIORef initial_state
    setProperty "x" sprite (floatAsVal $ initial_state.mouseX)
    setProperty "y" sprite (floatAsVal $ initial_state.mouseY)
    addChild app sprite
    fps_counter <- newText "0" "white"
    setProperty "x" fps_counter (floatAsVal 10.0)
    setProperty "y" fps_counter (floatAsVal 10.0)
    addChild app fps_counter
    addTicker app =<< jsFuncFromHs_ (rotateSprite sprite)

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

    -- Update fallSprite call to include paddle
    addTicker app =<< jsFuncFromHs_ (fallSprite state_ref (fromIntegral screen_width) (fromIntegral screen_height) sprite paddle)
