# Quick Wins - Immediate Improvements

## 1. Add Missing Type Signatures

### Files to Update: `src/Lib.hs`

```haskell
-- Line 239: Add explicit signature
jsFuncFromHs_ :: (JSVal -> IO ()) -> IO JSVal
jsFuncFromHs_ func = ...

-- Line 255: Add explicit signature  
getPropertyKey :: [JSString] -> JSVal -> IO JSVal
getPropertyKey keys obj = ...

-- Line 264: Add explicit signature
setPropertyKey :: [JSString] -> JSVal -> JSVal -> IO ()
setPropertyKey keys obj value = ...
```

## 2. Fix Potential Bug: Ball Speed Calculation

### Issue in `app/Test.hs` line 159
The `abs` function is applied incorrectly - it should preserve sign for x_speed:

```haskell
-- Current (line 159-160):
new_y_speed = y_direction * abs increased_speed * cos angle
new_x_speed = abs increased_speed * sin angle

-- Suggested fix:
new_y_speed = y_direction * increased_speed * cos angle
new_x_speed = increased_speed * sin angle
```

The `abs` on `increased_speed` removes the sign, but `sin angle` already provides the correct sign based on hit position.

## 3. Extract Magic Numbers

### Create `app/GameConfig.hs`:

```haskell
module GameConfig where

data GameConfig = GameConfig {
    ballRotationSpeed :: Float,
    maxBounceAngle :: Float,
    speedMultiplier :: Float,
    paddleCollisionThreshold :: Float,
    initialBallXSpeed :: Float,
    initialBallYSpeed :: Float,
    computerPaddleMaxSpeed :: Float,
    paddleWidth :: Float,
    paddleHeight :: Float,
    bottomPaddleOffset :: Float,
    topPaddleOffset :: Float,
    fpsCounterUpdateRate :: Int
}

defaultConfig :: GameConfig
defaultConfig = GameConfig {
    ballRotationSpeed = 0.01,
    maxBounceAngle = 1.0,
    speedMultiplier = 1.05,
    paddleCollisionThreshold = 20.0,
    initialBallXSpeed = 2.0,
    initialBallYSpeed = 5.0,
    computerPaddleMaxSpeed = 2.0,
    paddleWidth = 50.0,
    paddleHeight = 10.0,
    bottomPaddleOffset = 100.0,
    topPaddleOffset = 100.0,
    fpsCounterUpdateRate = 10
}
```

## 4. Improve Error Messages

### Add error handling wrapper in `src/Lib.hs`:

```haskell
-- Add after line 320
safeGetProperty :: JSString -> JSVal -> IO (Maybe JSVal)
safeGetProperty propName obj = do
    val <- getProperty propName obj
    -- Check if property exists (JavaScript returns undefined if missing)
    isUndefined <- foreign import javascript unsafe "$1 === undefined" val :: IO Bool
    if isUndefined then return Nothing else return (Just val)
```

## 5. Fix Inconsistent Naming

### Update constants in `app/Test.hs` to use camelCase:

```haskell
-- Replace snake_case with camelCase throughout
ballRotationSpeed :: Float  -- was: ball_rotation_speed
maxBounceAngle :: Float     -- was: max_bounce_angle
speedMultiplier :: Float    -- was: speed_multiplier
-- etc.
```

## 6. Add Input Validation

### In `app/Test.hs`, add bounds checking:

```haskell
-- Line 384: Add validation
setupPlayerPaddle :: JSVal -> JSVal -> Int -> IO ()
setupPlayerPaddle app paddle screen_width = do
    addEventListener "globalpointermove" paddle =<< jsFuncFromHs_
      (\event -> do
            mx <- valAsFloat <$> getPropertyKey ["screen", "x"] event
            let clampedX = max 0.0 $ min (fromIntegral screen_width) mx
            setProperty "x" paddle (floatAsVal clampedX)
      )
```

## 7. Optimize Property Access

### Cache paddle dimensions (they don't change):

```haskell
-- In main function, after creating paddles:
bottom_paddle_width <- valAsFloat <$> getProperty "width" bottom_paddle
top_paddle_width <- valAsFloat <$> getProperty "width" top_paddle
-- Store in IORef or pass as constants to updateGamePhysics
```

## 8. Add Missing Documentation

### Add to `updateBallState`:

```haskell
-- | Updates ball state based on physics and collisions.
--
-- Collision priority:
-- 1. Paddle collisions (checked first)
-- 2. Top/bottom edge (scoring)
-- 3. Left/right edge (bounce)
--
-- Edge case: If ball hits corner (paddle + edge simultaneously),
-- paddle collision takes priority.
updateBallState :: ...
```

## 9. Fix Potential Memory Leak

### In `setupFPSCounter`, ensure ticker is cleaned up:

```haskell
-- Consider storing ticker reference for cleanup if needed
-- Or document that ticker runs for lifetime of app
```

## 10. Improve Build Script Robustness

### Update `build-and-copy.sh`:

```bash
#!/bin/bash
set -euo pipefail  # Add -u and -o pipefail

# Add validation
if ! command -v docker &> /dev/null; then
    echo "Error: docker is not installed"
    exit 1
fi

# Add cleanup on error
trap 'echo "Build failed. Cleaning up..."' ERR
```
