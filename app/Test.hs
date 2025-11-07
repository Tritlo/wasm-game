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

data ScoreState = ScoreState {
    playerScore :: Int,
    computerScore :: Int
}

type Screen = (Float, Float)
type Paddle = (Float, Float, Float)

-- | Render the ball state to the sprite
renderBall :: BallState -> JSVal -> IO ()
renderBall ball sprite = do
    setProperty "x" sprite (floatAsVal $ ball.ballX)
    setProperty "y" sprite (floatAsVal $ ball.ballY)

-- | Calculate bounce velocity based on paddle hit position
-- hit_position: normalized position on paddle (-1.0 = left edge, 0.0 = center, 1.0 = right edge)
-- base_speed: base speed magnitude to maintain
-- is_top_paddle: True if bouncing off top paddle (ball should go down), False for bottom paddle (ball should go up)
-- Returns: (new_x_speed, new_y_speed)
calculatePaddleBounce :: Float -> Float -> Bool -> (Float, Float)
calculatePaddleBounce hit_position base_speed is_top_paddle =
    -- Maximum angle deviation (in radians) - adjust this to control how much angle changes
    let max_angle = 1.0  -- ~57 degrees
        angle = hit_position * max_angle
        -- Increase speed slightly on each bounce (5% increase)
        speed_multiplier = 1.05
        increased_speed = base_speed * speed_multiplier
        -- Calculate new velocities based on angle
        -- Y speed direction depends on which paddle: top paddle -> positive (down), bottom paddle -> negative (up)
        y_direction = if is_top_paddle then 1.0 else -1.0
        new_y_speed = y_direction * abs increased_speed * cos angle
        new_x_speed = abs increased_speed * sin angle
    in (new_x_speed, new_y_speed)

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

-- | Check collision with a paddle and return hit position
-- Returns: (collision_detected, hit_position)
-- hit_position: normalized position on paddle (-1.0 = left edge, 0.0 = center, 1.0 = right edge)
checkPaddleCollision :: Float -> Float -> Paddle -> Bool -> Bool -> (Bool, Float)
checkPaddleCollision ball_x ball_y (paddle_x, paddle_y, paddle_width) ball_moving_down paddle_is_bottom =
    let paddle_half_width = paddle_width / 2.0
        paddle_left = paddle_x - paddle_half_width
        paddle_right = paddle_x + paddle_half_width
        x_collision = ball_x >= paddle_left && ball_x <= paddle_right
        y_collision = abs (ball_y - paddle_y) < 20.0
        -- Bottom paddle: ball must be moving down
        -- Top paddle: ball must be moving up
        correct_direction = if paddle_is_bottom then ball_moving_down else not ball_moving_down
        collision = x_collision && y_collision && correct_direction
        -- Calculate normalized hit position (-1.0 to 1.0)
        hit_position = if collision then
            -- Distance from center of paddle, normalized to [-1, 1], clamped
            max (-1.0) $ min 1.0 $ (ball_x - paddle_x) / paddle_half_width
        else
            0.0
    in (collision, hit_position)

-- | Update ball state based on physics and collisions
-- Returns: (updated_ball_state, scoring_event, bounce_occurred)
-- scoring_event: Just True = player scored, Just False = computer scored, Nothing = no score
-- bounce_occurred: True if ball bounced (paddle or edge), False otherwise
updateBallState :: BallState -> Float -> Screen -> Paddle -> Paddle -> (BallState, Maybe Bool, Bool)
updateBallState ball dt (screen_width, screen_height) bottom_paddle top_paddle =
    let new_y = ball.ballY + ball.ballYSpeed * dt
        new_x = ball.ballX + ball.ballXSpeed * dt
        ball_moving_down = ball.ballYSpeed > 0.0
        (bottom_collision, bottom_hit_pos) = checkPaddleCollision new_x new_y bottom_paddle ball_moving_down True
        (top_collision, top_hit_pos) = checkPaddleCollision new_x new_y top_paddle ball_moving_down False
        -- Calculate base speed magnitude
        base_speed = sqrt (ball.ballXSpeed * ball.ballXSpeed + ball.ballYSpeed * ball.ballYSpeed)
    in
    -- Check for paddle collisions
    if bottom_collision then
        -- Bounce off bottom paddle with angle based on hit position (ball goes up)
        let (new_x_speed, new_y_speed) = calculatePaddleBounce bottom_hit_pos base_speed False
        in (ball { ballX = new_x, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else if top_collision then
        -- Bounce off top paddle with angle based on hit position (ball goes down)
        let (new_x_speed, new_y_speed) = calculatePaddleBounce top_hit_pos base_speed True
        in (ball { ballX = new_x, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else if new_y < 0.0 then
        -- Top edge: player scores, reset ball going toward player (downward)
        (ball { ballX = screen_width / 2.0, ballY = screen_height / 2.0, ballXSpeed = 2.0, ballYSpeed = 5.0 }, Just True, False)
    else if new_y > screen_height then
        -- Bottom edge: computer scores, reset ball going toward computer (upward)
        (ball { ballX = screen_width / 2.0, ballY = screen_height / 2.0, ballXSpeed = 2.0, ballYSpeed = -5.0 }, Just False, False)
    else if new_x < 0.0 then
        -- Left edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in (ball { ballX = 0.0, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else if new_x > screen_width then
        -- Right edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in (ball { ballX = screen_width, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else
        (ball { ballX = new_x, ballY = new_y }, Nothing, False)


-- | AI function to move computer paddle towards ball
updateComputerPaddle :: IORef BallState -> Float -> JSVal -> JSVal -> IO ()
updateComputerPaddle ball_state_ref screen_width computer_paddle ticker = do
    ball_state <- readIORef ball_state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker
    current_paddle_x <- valAsFloat <$> getProperty "x" computer_paddle
    let target_x = ball_state.ballX
        max_speed = 2.0 -- pixels per second (reduced to make it beatable)
        distance = target_x - current_paddle_x
        max_move = max_speed * dt
        move = if abs distance < max_move then distance else if distance > 0 then max_move else -max_move
        new_x = max 0.0 $ min (screen_width) (current_paddle_x + move)
    setProperty "x" computer_paddle (floatAsVal new_x)

-- | Update score display
updateScoreDisplay :: ScoreState -> JSVal -> IO ()
updateScoreDisplay score score_text = do
    let score_str = show score.playerScore ++ " - " ++ show score.computerScore
    setProperty "text" score_text (stringAsVal $ toJSString score_str)

-- | Move sprite downward with constant speed and respawn at center if it falls out
fallSprite :: IORef BallState -> IORef ScoreState -> Screen -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()
fallSprite ball_state_ref score_state_ref screen sprite bottom_paddle top_paddle score_text ticker = do
    ball_state <- readIORef ball_state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker
    bottom_paddle_x <- valAsFloat <$> getProperty "x" bottom_paddle
    bottom_paddle_y <- valAsFloat <$> getProperty "y" bottom_paddle
    bottom_paddle_width <- valAsFloat <$> getProperty "width" bottom_paddle
    top_paddle_x <- valAsFloat <$> getProperty "x" top_paddle
    top_paddle_y <- valAsFloat <$> getProperty "y" top_paddle
    top_paddle_width <- valAsFloat <$> getProperty "width" top_paddle
    let (updated_ball, scoring_event, bounce_occurred) = updateBallState ball_state dt screen
                                      (bottom_paddle_x, bottom_paddle_y, bottom_paddle_width)
                                      (top_paddle_x, top_paddle_y, top_paddle_width)
    writeIORef ball_state_ref updated_ball
    renderBall updated_ball sprite
    -- Play blip sound on bounce
    when bounce_occurred $ blip
    -- Handle scoring
    case scoring_event of
        Just True -> do  -- Player scored
            score <- readIORef score_state_ref
            let new_score = score { playerScore = score.playerScore + 1 }
            writeIORef score_state_ref new_score
            updateScoreDisplay new_score score_text
        Just False -> do  -- Computer scored
            score <- readIORef score_state_ref
            let new_score = score { computerScore = score.computerScore + 1 }
            writeIORef score_state_ref new_score
            updateScoreDisplay new_score score_text
        Nothing -> return ()



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
                                   ballXSpeed = 0.0,
                                   ballYSpeed = 5.0 }
        initial_state = State { mouseX = fromIntegral screen_width / 2.0,
                               mouseY = fromIntegral screen_height / 2.0 }
        initial_score = ScoreState { playerScore = 0, computerScore = 0 }
    ball_state_ref <- newIORef initial_ball
    state_ref <- newIORef initial_state
    score_state_ref <- newIORef initial_score
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

    -- Bottom paddle (player controlled)
    bottom_paddle <- baseTexture "WHITE" >>= newSprite
    setProperty "eventMode" bottom_paddle (stringAsVal "static")
    setProperty "width" bottom_paddle (floatAsVal 50.0)
    setProperty "height" bottom_paddle (floatAsVal 10.0)
    setAnchor bottom_paddle 0.5
    setProperty "x" bottom_paddle (floatAsVal $ (fromIntegral  screen_width) / 2.0)
    setProperty "y" bottom_paddle (floatAsVal $ (fromIntegral  screen_height) - 100.0)
    addChild app bottom_paddle

    addEventListener "globalpointermove" bottom_paddle =<< jsFuncFromHs_
      (\event -> do
                    mx <- valAsFloat <$> getPropertyKey ["screen", "x"] event
                    when (mx >= 0.0 && mx <= fromIntegral screen_width) $ do
                        setProperty "x" bottom_paddle (floatAsVal mx)
                    )

    -- Top paddle (computer controlled)
    top_paddle <- baseTexture "WHITE" >>= newSprite
    setProperty "eventMode" top_paddle (stringAsVal "static")
    setProperty "width" top_paddle (floatAsVal 50.0)
    setProperty "height" top_paddle (floatAsVal 10.0)
    setAnchor top_paddle 0.5
    setProperty "x" top_paddle (floatAsVal $ (fromIntegral  screen_width) / 2.0)
    setProperty "y" top_paddle (floatAsVal 100.0)
    addChild app top_paddle

    -- Score display (top right corner)
    score_text <- newText "0 - 0" "white"
    setProperty "x" score_text (floatAsVal $ fromIntegral screen_width - 100.0)
    setProperty "y" score_text (floatAsVal 10.0)
    addChild app score_text
    updateScoreDisplay initial_score score_text

    -- Update ball physics and computer paddle AI
    addTicker app =<< jsFuncFromHs_ (fallSprite ball_state_ref score_state_ref (fromIntegral screen_width, fromIntegral screen_height) sprite bottom_paddle top_paddle score_text)
    addTicker app =<< jsFuncFromHs_ (updateComputerPaddle ball_state_ref (fromIntegral screen_width) top_paddle)
