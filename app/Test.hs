{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- | Pong game implementation using PIXI.js and GHC WebAssembly backend.
--
-- This module implements a classic Pong game where:
-- - The player controls the bottom paddle with mouse movement or gamepad (left stick X axis)
-- - The computer AI controls the top paddle
-- - The ball bounces off paddles and walls with physics-based collision detection
-- - Scoring occurs when the ball passes a paddle
module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when)

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

-- *****************************************************************************
-- * Game Constants
-- *****************************************************************************

-- | Ball rotation speed multiplier (radians per second)
ball_rotation_speed :: Float
ball_rotation_speed = 0.01

-- | Maximum angle deviation for paddle bounces (in radians, ~57 degrees)
max_bounce_angle :: Float
max_bounce_angle = 1.0

-- | Speed multiplier applied on each paddle bounce (5% increase)
speed_multiplier :: Float
speed_multiplier = 1.05

-- | Collision detection threshold for paddle hits (pixels)
paddle_collision_threshold :: Float
paddle_collision_threshold = 20.0

-- | Initial ball X speed
initial_ball_x_speed :: Float
initial_ball_x_speed = 2.0

-- | Initial ball Y speed (positive = downward)
initial_ball_y_speed :: Float
initial_ball_y_speed = 5.0

-- | Computer paddle maximum movement speed (pixels per second)
computer_paddle_max_speed :: Float
computer_paddle_max_speed = 2.0

-- | Paddle dimensions
paddle_width :: Float
paddle_width = 50.0

paddle_height :: Float
paddle_height = 10.0

-- | Paddle positions (distance from edges)
bottom_paddle_offset :: Float
bottom_paddle_offset = 100.0

top_paddle_offset :: Float
top_paddle_offset = 100.0

-- | UI element positions
fps_counter_x :: Float
fps_counter_x = 10.0

fps_counter_y :: Float
fps_counter_y = 10.0

score_text_x_offset :: Float
score_text_x_offset = 100.0

score_text_y :: Float
score_text_y = 10.0

-- | Start message position offset (pixels above center)
start_message_y_offset :: Float
start_message_y_offset = 50.0

-- | FPS counter update rate (updates per second)
fps_counter_update_rate :: Int
fps_counter_update_rate = 10

-- | Sound frequencies for game events
player_score_sound_freq :: Float
player_score_sound_freq = 800.0

computer_score_sound_freq :: Float
computer_score_sound_freq = 200.0

-- *****************************************************************************
-- * Data Types
-- *****************************************************************************

-- | Represents the current state of the ball in the game.
data BallState = BallState {
    ballX :: Float,      -- ^ X position of the ball center
    ballY :: Float,      -- ^ Y position of the ball center
    ballXSpeed :: Float, -- ^ Horizontal velocity (pixels per second)
    ballYSpeed :: Float  -- ^ Vertical velocity (pixels per second)
}

-- | Represents the current score state of the game.
data ScoreState = ScoreState {
    playerScore :: Int,   -- ^ Player's score (bottom paddle)
    computerScore :: Int  -- ^ Computer's score (top paddle)
}

-- | Screen dimensions as (width, height) in pixels.
type Screen = (Float, Float)

-- | Paddle representation as (x, y, width) where x and y are center coordinates.
type Paddle = (Float, Float, Float)

-- *****************************************************************************
-- * Rendering Functions
-- *****************************************************************************

-- | Rotates the sprite by the delta time.
-- This is a visual effect that rotates the ball sprite continuously.
rotateSprite :: JSVal -> JSVal -> IO ()
rotateSprite sprite time = do
    dt <- valAsFloat <$> getProperty "deltaTime" time
    incrementProperty "rotation" sprite (floatAsVal $ dt * ball_rotation_speed)

-- | Renders the ball state to the sprite by updating its position.
--
-- @param ball The current ball state
-- @param sprite The PIXI sprite representing the ball
renderBall :: BallState -> JSVal -> IO ()
renderBall ball sprite = do
    setProperty "x" sprite (floatAsVal $ ball.ballX)
    setProperty "y" sprite (floatAsVal $ ball.ballY)

-- *****************************************************************************
-- * Physics and Collision Detection
-- *****************************************************************************

-- | Calculates bounce velocity based on paddle hit position.
--
-- The angle of bounce depends on where the ball hits the paddle:
-- - Hitting the center results in a straight bounce
-- - Hitting the edges results in an angled bounce
-- - Speed increases slightly on each bounce to increase difficulty
--
-- @param hit_position Normalized position on paddle (-1.0 = left edge, 0.0 = center, 1.0 = right edge)
-- @param base_speed Base speed magnitude to maintain
-- @param is_top_paddle True if bouncing off top paddle (ball should go down), False for bottom paddle (ball should go up)
-- @return (new_x_speed, new_y_speed) tuple
calculatePaddleBounce :: Float -> Float -> Bool -> (Float, Float)
calculatePaddleBounce hit_position base_speed is_top_paddle =
    let angle = hit_position * max_bounce_angle
        increased_speed = base_speed * speed_multiplier
        -- Y speed direction depends on which paddle: top paddle -> positive (down), bottom paddle -> negative (up)
        y_direction = if is_top_paddle then 1.0 else -1.0
        new_y_speed = y_direction * abs increased_speed * cos angle
        new_x_speed = abs increased_speed * sin angle
    in (new_x_speed, new_y_speed)

-- | Reflects velocity based on surface normal.
--
-- For horizontal surfaces (top/bottom edges), reflects the Y component.
-- For vertical surfaces (left/right edges), reflects the X component.
--
-- @param x_speed Current horizontal velocity
-- @param y_speed Current vertical velocity
-- @param is_horizontal True for horizontal surfaces (top/bottom), False for vertical surfaces (left/right)
-- @return (new_x_speed, new_y_speed) tuple
reflectVelocity :: Float -> Float -> Bool -> (Float, Float)
reflectVelocity x_speed y_speed is_horizontal =
    if is_horizontal then
        -- Reflect across horizontal axis: reverse Y, keep X
        (x_speed, -y_speed)
    else
        -- Reflect across vertical axis: reverse X, keep Y
        (-x_speed, y_speed)

-- | Checks collision with a paddle and returns hit position.
--
-- Determines if the ball has collided with a paddle and calculates the normalized
-- hit position for angle-based bouncing.
--
-- @param ball_x Ball's X position
-- @param ball_y Ball's Y position
-- @param paddle The paddle to check collision with (x, y, width)
-- @param ball_moving_down True if ball is moving downward, False if moving upward
-- @param paddle_is_bottom True if this is the bottom paddle, False if top paddle
-- @return (collision_detected, hit_position) where hit_position is normalized (-1.0 = left edge, 0.0 = center, 1.0 = right edge)
checkPaddleCollision :: Float -> Float -> Paddle -> Bool -> Bool -> (Bool, Float)
checkPaddleCollision ball_x ball_y (paddle_x, paddle_y, paddle_width) ball_moving_down paddle_is_bottom =
    let paddle_half_width = paddle_width / 2.0
        paddle_left = paddle_x - paddle_half_width
        paddle_right = paddle_x + paddle_half_width
        x_collision = ball_x >= paddle_left && ball_x <= paddle_right
        y_collision = abs (ball_y - paddle_y) < paddle_collision_threshold
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

-- | Updates ball state based on physics and collisions.
--
-- Handles ball movement, collision detection with paddles and walls,
-- and scoring events. Returns updated state along with game events.
--
-- @param ball Current ball state
-- @param dt Delta time (seconds) since last update
-- @param screen Screen dimensions (width, height)
-- @param bottom_paddle Player's paddle (bottom)
-- @param top_paddle Computer's paddle (top)
-- @return (updated_ball_state, scoring_event, bounce_occurred) where:
--         - scoring_event: Just True = player scored, Just False = computer scored, Nothing = no score
--         - bounce_occurred: True if ball bounced (paddle or edge), False otherwise
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
    -- Check for paddle collisions first (they take priority)
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
        (ball { ballX = screen_width / 2.0, ballY = screen_height / 2.0, ballXSpeed = initial_ball_x_speed, ballYSpeed = initial_ball_y_speed }, Just True, False)
    else if new_y > screen_height then
        -- Bottom edge: computer scores, reset ball going toward computer (upward)
        (ball { ballX = screen_width / 2.0, ballY = screen_height / 2.0, ballXSpeed = initial_ball_x_speed, ballYSpeed = -initial_ball_y_speed }, Just False, False)
    else if new_x < 0.0 then
        -- Left edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in (ball { ballX = 0.0, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else if new_x > screen_width then
        -- Right edge: bounce based on angle (reflect X component, preserve Y)
        let (new_x_speed, new_y_speed) = reflectVelocity ball.ballXSpeed ball.ballYSpeed False
        in (ball { ballX = screen_width, ballY = new_y, ballXSpeed = new_x_speed, ballYSpeed = new_y_speed }, Nothing, True)
    else
        -- No collision, just update position
        (ball { ballX = new_x, ballY = new_y }, Nothing, False)


-- *****************************************************************************
-- * AI Functions
-- *****************************************************************************

-- | AI function to move computer paddle towards the ball.
--
-- The computer paddle attempts to follow the ball's X position with a limited
-- speed to make the game beatable. The paddle moves smoothly towards the ball's
-- current X coordinate.
--
-- @param ball_state_ref Reference to the current ball state
-- @param screen_width Width of the screen (for boundary clamping)
-- @param computer_paddle The PIXI sprite representing the computer paddle
-- @param ticker The ticker object providing deltaTime
updateComputerPaddle :: IORef BallState -> Float -> JSVal -> JSVal -> IO ()
updateComputerPaddle ball_state_ref screen_width computer_paddle ticker = do
    ball_state <- readIORef ball_state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker
    current_paddle_x <- valAsFloat <$> getProperty "x" computer_paddle
    let target_x = ball_state.ballX
        distance = target_x - current_paddle_x
        max_move = computer_paddle_max_speed * dt
        move = if abs distance < max_move then distance else if distance > 0 then max_move else -max_move
        new_x = max 0.0 $ min screen_width (current_paddle_x + move)
    setProperty "x" computer_paddle (floatAsVal new_x)

-- *****************************************************************************
-- * Game Update Functions
-- *****************************************************************************

-- | Updates the score display text.
--
-- @param score Current score state
-- @param score_text The PIXI text object displaying the score
updateScoreDisplay :: ScoreState -> JSVal -> IO ()
updateScoreDisplay score score_text = do
    let score_str = show score.playerScore ++ " - " ++ show score.computerScore
    setProperty "text" score_text (stringAsVal $ toJSString score_str)

-- | Updates player paddle position based on gamepad input.
--
-- Polls gamepad state and updates paddle position using left stick X axis.
-- Falls back to current position if no gamepad input is detected.
--
-- @param paddle The player's paddle sprite
-- @param screen_width Screen width for boundary checking
-- @param dt Delta time for smooth movement
updatePlayerPaddle :: JSVal -> Float -> Float -> IO ()
updatePlayerPaddle paddle screen_width dt = do
    -- Poll gamepad state
    gamepad <- getFirstGamepad
    gamepad_x <- getGamepadAxis gamepad 0  -- Left stick X axis

    -- Apply deadzone to avoid drift
    let deadzone = 0.15
    let gamepad_x' = if abs gamepad_x < deadzone then 0.0 else gamepad_x

    -- Only update if gamepad input is active
    when (abs gamepad_x' > 0.0) $ do
        current_x <- valAsFloat <$> getProperty "x" paddle
        -- Gamepad movement speed (pixels per second per unit of axis input)
        -- Reduced significantly for less sensitive control
        let paddle_speed = 25.0
        let move = gamepad_x' * paddle_speed * dt
        let new_x = max 0.0 $ min screen_width (current_x + move)
        setProperty "x" paddle (floatAsVal new_x)

-- | Main game update function called on each tick.
--
-- Updates ball physics, handles collisions, renders the ball, plays sounds,
-- and manages scoring. This is the core game loop function.
--
-- @param ball_state_ref Reference to the current ball state
-- @param score_state_ref Reference to the current score state
-- @param screen Screen dimensions (width, height)
-- @param sprite The PIXI sprite representing the ball
-- @param bottom_paddle The player's paddle sprite
-- @param top_paddle The computer's paddle sprite
-- @param score_text The PIXI text object for score display
-- @param ticker The ticker object providing deltaTime
updateGamePhysics :: IORef BallState -> IORef ScoreState -> Screen -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()
updateGamePhysics ball_state_ref score_state_ref screen sprite bottom_paddle top_paddle score_text ticker = do
    ball_state <- readIORef ball_state_ref
    dt <- valAsFloat <$> getProperty "deltaTime" ticker

    -- Update player paddle with gamepad input
    let (screen_width, _) = screen
    updatePlayerPaddle bottom_paddle screen_width dt

    -- Get paddle positions and dimensions
    bottom_paddle_x <- valAsFloat <$> getProperty "x" bottom_paddle
    bottom_paddle_y <- valAsFloat <$> getProperty "y" bottom_paddle
    bottom_paddle_width <- valAsFloat <$> getProperty "width" bottom_paddle
    top_paddle_x <- valAsFloat <$> getProperty "x" top_paddle
    top_paddle_y <- valAsFloat <$> getProperty "y" top_paddle
    top_paddle_width <- valAsFloat <$> getProperty "width" top_paddle
    -- Update ball physics
    let (updated_ball, scoring_event, bounce_occurred) = updateBallState ball_state dt screen
                                      (bottom_paddle_x, bottom_paddle_y, bottom_paddle_width)
                                      (top_paddle_x, top_paddle_y, top_paddle_width)
    -- Update state and render
    writeIORef ball_state_ref updated_ball
    renderBall updated_ball sprite
    -- Play sound effects
    when bounce_occurred $ blip
    -- Handle scoring
    case scoring_event of
        Just True -> do  -- Player scored
            blipWithFreq player_score_sound_freq
            score <- readIORef score_state_ref
            let new_score = score { playerScore = score.playerScore + 1 }
            writeIORef score_state_ref new_score
            updateScoreDisplay new_score score_text
        Just False -> do  -- Computer scored
            blipWithFreq computer_score_sound_freq
            score <- readIORef score_state_ref
            let new_score = score { computerScore = score.computerScore + 1 }
            writeIORef score_state_ref new_score
            updateScoreDisplay new_score score_text
        Nothing -> return ()



-- *****************************************************************************
-- * Initialization Functions
-- *****************************************************************************

-- | Creates and configures a paddle sprite.
--
-- @param app The PIXI application
-- @param x X position (center)
-- @param y Y position (center)
-- @return The configured paddle sprite
createPaddle :: JSVal -> Float -> Float -> IO JSVal
createPaddle app x y = do
    paddle <- baseTexture "WHITE" >>= newSprite
    setProperty "eventMode" paddle (stringAsVal "static")
    setProperty "width" paddle (floatAsVal paddle_width)
    setProperty "height" paddle (floatAsVal paddle_height)
    setAnchor paddle 0.5
    setProperty "x" paddle (floatAsVal x)
    setProperty "y" paddle (floatAsVal y)
    addChild app paddle
    return paddle

-- | Sets up the player paddle with mouse and gamepad control.
--
-- @param app The PIXI application
-- @param paddle The paddle sprite to control
-- @param screen_width Screen width for boundary checking
setupPlayerPaddle :: JSVal -> JSVal -> Int -> IO ()
setupPlayerPaddle app paddle screen_width = do
    -- Mouse control
    addEventListener "globalpointermove" paddle =<< jsFuncFromHs_
      (\event -> do
            mx <- valAsFloat <$> getPropertyKey ["screen", "x"] event
            when (mx >= 0.0 && mx <= fromIntegral screen_width) $ do
                setProperty "x" paddle (floatAsVal mx)
      )

-- | Creates and configures the FPS counter display.
--
-- @param app The PIXI application
-- @return The FPS counter text sprite
setupFPSCounter :: JSVal -> IO JSVal
setupFPSCounter app = do
    fps_counter <- newText "0" "white"
    setProperty "x" fps_counter (floatAsVal fps_counter_x)
    setProperty "y" fps_counter (floatAsVal fps_counter_y)
    addChild app fps_counter

    -- Create a separate ticker for FPS updates (lower frequency)
    fps_ticker <- newTicker
    setProperty "maxFPS" fps_ticker (intAsVal fps_counter_update_rate)
    startTicker fps_ticker
    callAddTicker fps_ticker =<< jsFuncFromHs_ (\_ -> do
            fps <- fmap valAsFloat $ getPropertyKey ["ticker", "FPS"] app
            let fps_val = floor fps
            setProperty "text" fps_counter (stringAsVal $ toJSString $ show fps_val)
        )
    return fps_counter

-- | Creates and configures the score display.
--
-- @param app The PIXI application
-- @param screen_width Screen width for positioning
-- @param initial_score Initial score state
-- @return The score text sprite
setupScoreDisplay :: JSVal -> Int -> ScoreState -> IO JSVal
setupScoreDisplay app screen_width initial_score = do
    score_text <- newText "0 - 0" "white"
    setProperty "x" score_text (floatAsVal $ fromIntegral screen_width - score_text_x_offset)
    setProperty "y" score_text (floatAsVal score_text_y)
    addChild app score_text
    updateScoreDisplay initial_score score_text
    return score_text

-- | Creates and configures the "Click to start" message.
--
-- @param app The PIXI application
-- @param screen_width Screen width for centering
-- @param screen_height Screen height for centering
-- @param game_ticker The game ticker to start on click
-- @return The start message text sprite
setupStartMessage :: JSVal -> Int -> Int -> JSVal -> IO JSVal
setupStartMessage app screen_width screen_height game_ticker = do
    start_text <- newText "Click to start" "white"
    setProperty "eventMode" start_text (stringAsVal "static")
    setAnchor start_text 0.5
    setProperty "x" start_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" start_text (floatAsVal $ fromIntegral screen_height / 2.0 - start_message_y_offset)
    setProperty "hitArea" start_text =<< getProperty "screen" app
    addChild app start_text

    -- Start game on pointerdown
    addEventListener "pointerdown" start_text =<< jsFuncFromHs_
     (\_ -> do
            startTicker game_ticker
            setProperty "text" start_text $ stringAsVal $ toJSString "")
    return start_text

-- | Initializes the game state with default values.
--
-- @param screen_width Screen width
-- @param screen_height Screen height
-- @return (initial_ball_state, initial_score_state)
initializeGameState :: Int -> Int -> (BallState, ScoreState)
initializeGameState screen_width screen_height =
    let initial_ball = BallState {
            ballX = fromIntegral screen_width / 2.0,
            ballY = fromIntegral screen_height / 2.0,
            ballXSpeed = 0.0,
            ballYSpeed = initial_ball_y_speed
        }
        initial_score = ScoreState {
            playerScore = 0,
            computerScore = 0
        }
    in (initial_ball, initial_score)

-- *****************************************************************************
-- * Main Function
-- *****************************************************************************

-- | Main entry point for the Pong game.
--
-- Initializes the PIXI.js application, creates all game objects,
-- sets up event handlers, and configures the game loop.
main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp
    app <- initAppInTarget app "black" "#canvas-container"
    appendToTarget "#canvas-container" app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen

    -- Load ball sprite
    let ball_image_url = "./assets/images/logos/hf-logo-100-alpha.png"
    sprite <- loadAsset ball_image_url >>= newSprite
    setProperty "eventMode" sprite (stringAsVal "static")
    setAnchor sprite 0.5

    -- Initialize game state
    let (initial_ball, initial_score) = initializeGameState screen_width screen_height
    ball_state_ref <- newIORef initial_ball
    score_state_ref <- newIORef initial_score

    -- Render initial ball position
    renderBall initial_ball sprite
    addChild app sprite

    -- Add ball rotation effect
    addTicker app =<< jsFuncFromHs_ (rotateSprite sprite)

    -- Setup UI elements
    _fps_counter <- setupFPSCounter app
    score_text <- setupScoreDisplay app screen_width initial_score

    -- Create paddles
    let screen_width_f = fromIntegral screen_width
        screen_height_f = fromIntegral screen_height
    bottom_paddle <- createPaddle app (screen_width_f / 2.0) (screen_height_f - bottom_paddle_offset)
    top_paddle <- createPaddle app (screen_width_f / 2.0) top_paddle_offset

    -- Setup player paddle controls
    setupPlayerPaddle app bottom_paddle screen_width

    -- Setup game ticker (not started yet - waits for user click)
    game_ticker <- newTicker
    let screen = (screen_width_f, screen_height_f)
    callAddTicker game_ticker =<< jsFuncFromHs_ (updateGamePhysics ball_state_ref score_state_ref screen sprite bottom_paddle top_paddle score_text)
    callAddTicker game_ticker =<< jsFuncFromHs_ (updateComputerPaddle ball_state_ref screen_width_f top_paddle)

    -- Setup start message
    _start_text <- setupStartMessage app screen_width screen_height game_ticker
    return ()
