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

data BallState = BallState {
    ballX :: Float,
    ballY :: Float,
    ballXSpeed :: Float,
    ballYSpeed :: Float
}

data State = State {
    mouseX :: Float,
    mouseY :: Float
}

type Screen = (Float, Float)
type Paddle = (Float, Float, Float)

-- | Render the ball state to the sprite
renderBall :: BallState -> JSVal -> IO ()
renderBall ball sprite = do
    setProperty "x" sprite (floatAsVal $ ball.ballX)
    setProperty "y" sprite (floatAsVal $ ball.ballY)

-- | Reflect velocity based on surface normal
-- For horizontal surfaces (top/bottom), reflect Y component
-- For vertical surfaces (left/right), reflect X component
reflectVelocity :: Float -> Float -> Bool -> (Float, Float)
reflectVelocity x_speed y_speed is_horizontal =
    if is_horizontal then
        -- Reflect across horizontal axis: reverse Y, keep X
        (x_speed, -y_speed)
    else
        -- Reflect across vertical axis: reverse X, keep Y
        (-x_speed, y_speed)

-- | Update ball state based on physics and collisions
updateBallState :: BallState -> Float -> Screen -> Paddle -> BallState
updateBallState ball dt (screen_width, screen_height) (paddle_x, paddle_y, paddle_width) =
    let new_y = ball.ballY + ball.ballYSpeed * dt
        new_x = ball.ballX + ball.ballXSpeed * dt
        paddle_half_width = paddle_width / 2.0
        paddle_left = paddle_x - paddle_half_width
        paddle_right = paddle_x + paddle_half_width
        -- Check if sprite x is within paddle x ± half width
        x_collision = new_x >= paddle_left && new_x <= paddle_right
        -- Check if sprite is at paddle y level (with some tolerance)
        y_collision = abs (new_y - paddle_y) < 20.0 && ball.ballYSpeed > 0.0
    in
    -- Check for paddle collision
    if x_collision && y_collision then
        -- Bounce off paddle: reverse y speed
        ball { ballX = new_x, ballY = new_y, ballYSpeed = -ball.ballYSpeed }
    else if new_y < 0.0 then
        -- Top edge: bounce based on angle (reflect Y component, preserve X)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed True
        in ball { ballX = new_x, ballY = 0.0, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }
    else if new_y > screen_height then
        -- Bottom edge: reset ball to center
        ball { ballX = screen_width / 2.0, ballY = screen_height / 2.0, ballXSpeed = 2.0, ballYSpeed = 5.0 }
    else if new_x < 0.0 then
        -- Left edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in ball { ballX = 0.0, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }
    else if new_x > screen_width then
        -- Right edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in ball { ballX = screen_width, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }
    else
        ball { ballX = new_x, ballY = new_y }


-- | Move sprite downward with constant speed and respawn at center if it falls out
fallSprite :: IORef BallState -> Screen -> JSVal -> JSVal -> JSVal -> IO ()
fallSprite ball_state_ref screen sprite paddle ticker = do
    ball_state <- readIORef ball_state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker
    paddle_x <- valAsFloat <$> getProperty "x" paddle
    paddle_y <- valAsFloat <$> getProperty "y" paddle
    paddle_width <- valAsFloat <$> getProperty "width" paddle
    let updated_ball = updateBallState ball_state dt screen (paddle_x,paddle_y, paddle_width)
    writeIORef ball_state_ref updated_ball
    renderBall updated_ball sprite



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
    let initial_ball = BallState { ballX = fromIntegral screen_width / 2.0,
                                   ballY = fromIntegral screen_height / 2.0,
                                   ballXSpeed = 2.0,
                                   ballYSpeed = 5.0 }
        initial_state = State { mouseX = fromIntegral screen_width / 2.0,
                               mouseY = fromIntegral screen_height / 2.0 }
    ball_state_ref <- newIORef initial_ball
    state_ref <- newIORef initial_state
    renderBall initial_ball sprite
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
    addTicker app =<< jsFuncFromHs_ (fallSprite ball_state_ref (fromIntegral screen_width, fromIntegral screen_height) sprite paddle)
