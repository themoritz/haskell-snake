{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State

import           System.Random

import           Data.Foldable       (find, for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified UI.NCurses          as N

import           Types

-- | The game state.
data Game = Game
  { _gameSnake     :: Snake
  , _gameDirection :: Direction
  , _gameFood      :: Point
  , _gameToGrow    :: Integer
  , _gameRunning   :: Bool
  }

makeLenses ''Game

-- | User actions.
data Action
  = Go Direction
  -- ^ Triggered by the arrow keys
  | Start
  -- ^ Triggered by the space key

--------------------------------------------------------------------------------
-- Utils

-- | Create a new game state with a snake of length 10 sitting in the middle of
-- the screen and facing to the left.
newGame
  :: Point
  -- ^ Dimensions of the window
  -> Point
  -- ^ Position of the first food
  -> Game
newGame (Point w h) food =
    Game (mkSnake head 10) DirLeft food 0 False
  where
    head = Point (w `div` 2) (h `div` 2)

-- | Normalize an arbitrary point to window coordinates.
toWindowCoords
  :: Point
  -- ^ Window dimensions (width, height)
  -> Point
  -- ^ The point
  -> Point
toWindowCoords (Point wx wy) (Point x y) = Point (x `mod` wx) (y `mod` wy)

-- | Move a point in the given direction, and normalize to window coordinates.
movePoint :: Point -> Direction -> Point -> Point
movePoint (Point x y) dir windowDims =
  toWindowCoords windowDims $ case dir of
    DirUp    -> Point x (y-1)
    DirDown  -> Point x (y+1)
    DirLeft  -> Point (x-1) y
    DirRight -> Point (x+1) y

--------------------------------------------------------------------------------
-- Game logic

applyAction :: MonadState Game m => Action -> m ()
applyAction action = case action of
  Go dir -> do
    currentDir <- use gameDirection
    when (dir /= opposite currentDir) $ gameDirection .= dir
  Start -> gameRunning .= True

stepFrame
  :: MonadState Game m
  => m Point
  -- ^ A monadic action that returns a new random point.
  -> Point
  -- ^ Dimensions of the window.
  -> m ()
stepFrame genRandomPoint windowDim = do
  running <- use gameRunning
  when running $ do
    oldHead <- use (gameSnake . snakeHead)
    direction <- use gameDirection
    snake <- use gameSnake
    food <- use gameFood
    let newHead = movePoint oldHead direction windowDim
    if elem newHead snake
      then put (newGame windowDim food)
      else do
        gameSnake %= snakePushHead newHead
        toGrow <- use gameToGrow
        when (newHead == food) $ do
          gameFood <~ genRandomPoint
          gameToGrow += 10
        if (toGrow == 0)
          then gameSnake %= snakePopTail
          else gameToGrow -= 1

--------------------------------------------------------------------------------
-- Renderer

renderGameState :: Game -> N.Update ()
renderGameState state = do
  let snake = state ^. gameSnake
  N.moveCursor 0 0
  N.drawString $ show $ length snake
  for_ snake $ \(Point x y) -> do
    N.moveCursor y x
    N.drawString "#"
  let Point x y = state ^. gameFood
  N.moveCursor y x
  N.drawString "*"

--------------------------------------------------------------------------------
-- Wiring everything up

-- | Extract an 'Action' from an ncurses event
eventToAction :: N.Event -> Maybe Action
eventToAction (N.EventSpecialKey key) = case key of
  N.KeyUpArrow    -> Just $ Go DirUp
  N.KeyDownArrow  -> Just $ Go DirDown
  N.KeyLeftArrow  -> Just $ Go DirLeft
  N.KeyRightArrow -> Just $ Go DirRight
  _               -> Nothing
eventToAction (N.EventCharacter char) = case char of
  ' ' -> Just Start
  _   -> Nothing
eventToAction _ = Nothing

gameLoop :: N.Window -> StateT Game N.Curses ()
gameLoop window = do
  (h, w) <- lift $ N.updateWindow window N.windowSize
  ev <- lift $ N.getEvent window (Just 0)
  let genRandomPoint = liftIO $ randomRIO (Point 0 0, Point (w-1) (h-1))
  case ev >>= eventToAction of
    Nothing     -> pure ()
    Just action -> applyAction action
  stepFrame genRandomPoint (Point w h)
  game <- get
  lift $ do
    N.updateWindow window $ do
      N.clear
      renderGameState game
    N.render
  liftIO $ threadDelay 50000

main :: IO ()
main = N.runCurses $ do
  N.setCursorMode N.CursorInvisible
  window <- N.defaultWindow
  (h, w) <- N.updateWindow window N.windowSize
  startFood <- liftIO $ randomRIO (Point 0 0, Point (w-1) (h-1))
  void $ runStateT
    (forever $ gameLoop window)
    (newGame (Point w h) startFood)
