# Code Review and Improvement Suggestions

## Overview
This is a well-structured Haskell WebAssembly project implementing a Pong game using PIXI.js. The code is generally clean and well-documented. Below are suggestions for improvements organized by category.

---

## 1. Code Quality & Type Safety

### 1.1 Missing Type Signatures
**Issue**: Some helper functions lack explicit type signatures, making code harder to understand and maintain.

**Location**: `src/Lib.hs`, `app/Test.hs`

**Recommendation**: Add explicit type signatures for all exported and internal functions.

**Example**:
```haskell
-- Current (implicit)
jsFuncFromHs_ func = ...

-- Suggested
jsFuncFromHs_ :: (JSVal -> IO ()) -> IO JSVal
jsFuncFromHs_ func = ...
```

### 1.2 Use Newtype Wrappers for Semantic Types
**Issue**: Using raw `Float` and `Int` types for different concepts reduces type safety.

**Location**: `app/Test.hs`

**Recommendation**: Create newtype wrappers for semantic types:
```haskell
newtype Pixels = Pixels Float deriving (Eq, Ord, Num, Fractional, Show)
newtype Speed = Speed Float deriving (Eq, Ord, Num, Fractional, Show)
newtype Radians = Radians Float deriving (Eq, Ord, Num, Fractional, Show)
newtype Seconds = Seconds Float deriving (Eq, Ord, Num, Fractional, Show)
```

This prevents accidentally mixing units (e.g., passing pixels where speed is expected).

### 1.3 Replace Tuple Types with Record Types
**Issue**: Using tuples for `Screen` and `Paddle` reduces readability.

**Location**: `app/Test.hs` lines 113-116

**Recommendation**:
```haskell
data Screen = Screen {
    screenWidth :: Float,
    screenHeight :: Float
}

data Paddle = Paddle {
    paddleX :: Float,
    paddleY :: Float,
    paddleWidth :: Float
}
```

---

## 2. Error Handling

### 2.1 Missing Error Handling for FFI Calls
**Issue**: JavaScript FFI calls can fail, but errors aren't handled.

**Location**: `src/Lib.hs` - all `foreign import javascript` declarations

**Recommendation**: Add error handling wrappers:
```haskell
-- Safe wrapper for FFI calls
safeFFICall :: IO JSVal -> IO (Maybe JSVal)
safeFFICall action = catch (Just <$> action) (\_ -> return Nothing)

-- Or use Either for better error messages
safeFFICallEither :: IO JSVal -> IO (Either String JSVal)
safeFFICallEither action = catch (Right <$> action) (return . Left . show)
```

### 2.2 Validate JavaScript Values Before Conversion
**Issue**: Type conversion functions (`valAsFloat`, `valAsInt`, etc.) are unsafe and can crash.

**Location**: `src/Lib.hs` lines 286-304

**Recommendation**: Add validation:
```haskell
valAsFloatSafe :: JSVal -> IO (Maybe Float)
valAsFloatSafe val = do
    -- Check if value is a number in JavaScript
    isNumber <- foreign import javascript unsafe "typeof $1 === 'number'"
    if isNumber then Just <$> valAsFloat val else return Nothing
```

---

## 3. Performance Optimizations

### 3.1 Reduce Redundant Property Access
**Issue**: Paddle properties are read every frame in `updateGamePhysics`.

**Location**: `app/Test.hs` lines 319-324

**Recommendation**: Cache paddle positions or use a more efficient data structure:
```haskell
-- Cache paddle state in Haskell
data PaddleState = PaddleState {
    paddleSprite :: JSVal,
    paddleX :: IORef Float,
    paddleY :: Float,  -- constant
    paddleWidth :: Float  -- constant
}
```

### 3.2 Optimize FPS Counter Updates
**Issue**: FPS counter creates a separate ticker, which may be unnecessary.

**Location**: `app/Test.hs` lines 392-408

**Recommendation**: Update FPS counter less frequently using a counter:
```haskell
updateFPSCounter :: IORef Int -> JSVal -> JSVal -> IO ()
updateFPSCounter frame_count_ref app fps_counter = do
    count <- readIORef frame_count_ref
    if count `mod` fps_counter_update_rate == 0 then do
        fps <- fmap valAsFloat $ getPropertyKey ["ticker", "FPS"] app
        setProperty "text" fps_counter (stringAsVal $ toJSString $ show $ floor fps)
    else return ()
    writeIORef frame_count_ref (count + 1)
```

### 3.3 Batch DOM/Property Updates
**Issue**: Multiple property updates could be batched.

**Recommendation**: Create a batch update function for related properties.

---

## 4. Code Organization

### 4.1 Split Large Module
**Issue**: `app/Test.hs` is 526 lines and handles multiple concerns.

**Recommendation**: Split into modules:
- `Game.Physics` - collision detection and physics
- `Game.Rendering` - rendering functions
- `Game.AI` - computer paddle AI
- `Game.State` - game state management
- `Game.Initialization` - setup functions

### 4.2 Extract Constants to Separate Module
**Issue**: Constants are mixed with code logic.

**Location**: `app/Test.hs` lines 24-92

**Recommendation**: Create `Game.Constants` module:
```haskell
module Game.Constants where

ballRotationSpeed :: Float
ballRotationSpeed = 0.01
-- ... etc
```

### 4.3 Create Configuration Type
**Issue**: Many magic numbers scattered throughout.

**Recommendation**: Create a configuration record:
```haskell
data GameConfig = GameConfig {
    ballRotationSpeed :: Float,
    maxBounceAngle :: Float,
    speedMultiplier :: Float,
    paddleCollisionThreshold :: Float,
    -- ... etc
}

defaultConfig :: GameConfig
defaultConfig = GameConfig { ... }
```

---

## 5. Documentation Improvements

### 5.1 Add Module-Level Documentation
**Issue**: Missing high-level module documentation.

**Recommendation**: Add comprehensive module docs explaining architecture and design decisions.

### 5.2 Document Edge Cases
**Issue**: Some functions don't document edge cases (e.g., what happens when ball hits corner).

**Location**: `app/Test.hs` - `updateBallState` function

**Recommendation**: Document collision priority and edge case handling.

### 5.3 Add Usage Examples
**Issue**: No examples of how to use the library functions.

**Recommendation**: Add Haddock examples to key functions in `src/Lib.hs`.

---

## 6. Testing

### 6.1 Add Unit Tests
**Issue**: No tests present.

**Recommendation**: Add tests for pure functions:
- `calculatePaddleBounce`
- `reflectVelocity`
- `checkPaddleCollision`
- `updateBallState` (with mocked dependencies)

### 6.2 Add Property-Based Tests
**Recommendation**: Use QuickCheck for physics functions:
```haskell
prop_bounce_preserves_speed :: Float -> Float -> Bool -> Property
prop_bounce_preserves_speed hit_pos base_speed is_top =
    let (x_speed, y_speed) = calculatePaddleBounce hit_pos base_speed is_top
        new_speed = sqrt (x_speed^2 + y_speed^2)
    in abs (new_speed - base_speed * speed_multiplier) < 0.01
```

---

## 7. Build System & Scripts

### 7.1 Add Error Handling to Build Scripts
**Issue**: `build-and-copy.sh` could fail silently in some cases.

**Location**: `build-and-copy.sh`

**Recommendation**: Add more robust error checking:
```bash
set -euo pipefail  # Exit on error, undefined vars, pipe failures
```

### 7.2 Add Version Pinning
**Issue**: Dockerfile uses `ubuntu:noble` which may change.

**Location**: `Dockerfile` line 4

**Recommendation**: Pin to specific version or use SHA digest.

### 7.3 Add Build Cache Optimization
**Issue**: Docker build could be optimized with better layer caching.

**Recommendation**: Reorder Dockerfile instructions to maximize cache hits.

---

## 8. Security & Best Practices

### 8.1 Validate External URLs
**Issue**: Hardcoded external URL for ball image.

**Location**: `app/Test.hs` line 487

**Recommendation**: 
- Use environment variable or config file
- Add URL validation
- Consider hosting assets locally

### 8.2 Sanitize CSS Selectors
**Issue**: CSS selectors passed to JavaScript aren't validated.

**Location**: `src/Lib.hs` - `initAppInTarget`, `appendToTarget`

**Recommendation**: Add validation or use a safer API.

---

## 9. Code Style & Consistency

### 9.1 Consistent Naming Conventions
**Issue**: Mix of snake_case and camelCase for constants.

**Location**: `app/Test.hs`

**Recommendation**: Use consistent naming (prefer camelCase for Haskell):
```haskell
ballRotationSpeed :: Float  -- instead of ball_rotation_speed
```

### 9.2 Format Code Consistently
**Recommendation**: Use `ormolu` or `fourmolu` for consistent formatting.

### 9.3 Remove Unused Imports
**Issue**: `Data.String` import may not be needed everywhere.

**Recommendation**: Use `-Wunused-imports` flag and remove unused imports.

---

## 10. Game Logic Improvements

### 10.1 Add Game State Machine
**Issue**: Game state (menu, playing, paused, game over) isn't explicitly modeled.

**Recommendation**: Use a state machine:
```haskell
data GameState = Menu | Playing | Paused | GameOver
```

### 10.2 Add Pause Functionality
**Recommendation**: Allow pausing the game with spacebar or ESC.

### 10.3 Improve AI Difficulty
**Issue**: AI speed is constant.

**Recommendation**: Make AI difficulty adaptive or configurable.

### 10.4 Add Ball Trail Effect
**Recommendation**: Add visual trail for better gameplay feedback.

---

## 11. Accessibility

### 11.1 Add Keyboard Controls
**Issue**: Only mouse control available.

**Recommendation**: Add arrow keys or WASD for paddle control.

### 11.2 Add Screen Reader Support
**Recommendation**: Add ARIA labels and live regions for score updates.

---

## 12. Browser Compatibility

### 12.1 Add Feature Detection
**Issue**: `index.html` checks for WebAssembly but not for other required features.

**Recommendation**: Check for:
- AudioContext support
- PIXI.js availability
- Canvas support

---

## Priority Recommendations (High Impact, Low Effort)

1. **Add type signatures** - Improves code clarity and catches errors early
2. **Extract constants** - Makes configuration easier
3. **Add error handling** - Prevents runtime crashes
4. **Split large module** - Improves maintainability
5. **Add unit tests** - Ensures correctness during refactoring

---

## Summary

The codebase is well-written and functional. The main areas for improvement are:
- Better type safety through newtypes and explicit signatures
- Improved error handling
- Code organization through module splitting
- Addition of tests
- Performance optimizations for property access

Most suggestions are incremental improvements that can be implemented gradually without breaking existing functionality.
