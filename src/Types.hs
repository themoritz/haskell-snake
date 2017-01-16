{-# LANGUAGE TemplateHaskell #-}
-- |

module Types
  ( Point(..)
  , Snake
  , snakeHead
  , mkSnake
  , snakePushHead
  , snakePopTail
  , Direction(..)
  , opposite
  ) where

import           Control.Lens
import           System.Random

import           Data.Monoid

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------

data Point = Point
  { x :: Integer
  , y :: Integer
  } deriving (Eq, Show)

instance Random Point where
  randomR (Point minx miny, Point maxx maxy) g =
    let (x, g')  = randomR (minx, maxx) g
        (y, g'') = randomR (miny, maxy) g'
    in (Point x y, g'')
  random = randomR (Point 0 0, Point 100 100)

--------------------------------------------------------------------------------

-- | A snake with polymorphic body content.
data PolySnake a = Snake
  { _snakeHead :: a
  , snakeTail  :: Seq a
  } deriving (Show)

-- | Snakes are foldable! Use that to your advantage.
instance Foldable PolySnake where
  foldMap f (Snake h t) = f h <> foldMap f t

makeLenses ''PolySnake

-- | A snake consisting of points.
type Snake = PolySnake Point

-- | Create a new snake that faces to the left.
mkSnake
  :: Point
  -- ^ The head of the snake
  -> Integer
  -- ^ How long the snake should be
  -> Snake
mkSnake head len = Snake head $ Seq.unfoldr step 1
  where step i = if i >= len then Nothing
                            else Just (head { x = x head + i }, i+1)

-- | Make the snake longer by pushing a new head to the front of it.
snakePushHead :: Point -> Snake -> Snake
snakePushHead newHead (Snake h t) = Snake newHead (h Seq.<| t)

-- | Make the snake shorter by popping an element off its tail. If the snake has
-- length one, nothing happens.
snakePopTail :: Snake -> Snake
snakePopTail (Snake h t) = Snake h $ case Seq.viewr t of
  Seq.EmptyR    -> t
  rest Seq.:> _ -> rest

--------------------------------------------------------------------------------

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq)

-- | Get the opposite of a direction.
opposite :: Direction -> Direction
opposite DirUp    = DirDown
opposite DirDown  = DirUp
opposite DirLeft  = DirRight
opposite DirRight = DirLeft
