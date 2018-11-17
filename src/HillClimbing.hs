{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module HillClimbing where

import           Control.Comonad.Store
import           Data.Bifunctor
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Function
import           Data.Foldable
import Control.Arrow ((&&&))


-- a simple 2D wavy topology
-- graph here: http://gnuplot.sourceforge.net/demo_5.0/surface1.14.png
buildTopology :: (Float, Float) -> Store (Float, Float) Float
buildTopology startPos = store polynomial startPos
  where polynomial (x, y) = sin x * cos y

-- A functor representing nearby orthogonal directions
data Nearby a = Nearby {up :: a, right :: a, down :: a, left :: a}
  deriving (Functor, Show, Eq, Foldable)

-- Index type into 'Nearby'
data Dir = U | D | R | L deriving (Eq, Show)

instance Distributive Nearby where
  distribute = distributeRep

-- Nearby is indexable by Dir
instance Representable Nearby where
  type Rep Nearby = Dir
  index n U = up n
  index n D = down n
  index n R = right n
  index n L = left n

  tabulate f = Nearby {up=f U, down=f D, right=f R, left=f L}

-- Given a delta and a current position, fill the slots of Nearby with the
-- indexes of value which lives in that direction offset by delta
nearby :: Float -> (Float, Float) -> Nearby (Float, Float)
nearby = distribute . tabulate . move

-- Move an index by delta in the given direction
move :: Float -> Dir -> (Float, Float) -> (Float, Float)
move delta U = second (subtract delta)
move delta D = second (+ delta)
move delta L = first (subtract delta)
move delta R = first (+ delta)

-- Hill-climb one step by `delta` amount
step :: Float -> Store (Float, Float) Float -> Store (Float, Float) Float
step delta w =
  let nearbyValues = w & experiment (nearby delta)
      bestDirection =
        fst . maximumBy (compare `on` snd) . withIndex $ nearbyValues
  in  flip seek w $ move delta bestDirection (pos w)
  where withIndex = imapRep (,)

-- Hill-climb by `n` steps
steps
  :: Float -> Int -> Store (Float, Float) Float -> Store (Float, Float) Float
steps delta n w = nTimes n (step delta) w

-- Collect intermediate results as we step
showSteps
  :: Float -> Int -> Store (Float, Float) Float -> [((Float, Float), Float)]
showSteps delta n w = (pos &&& extract) <$> (take n $ iterate (step delta) w)

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = head . drop n . iterate f
