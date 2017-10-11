-- |
-- Module      : Data.AABB
-- Description : Defines types and operations on axis-aligned bounding boxes
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : ?
-- Portability : ?
--

-- TODO | - Encode handedness of coordinate system using types (eg. a type parameter) (?)
--        - Enforce lo < hi
--        - Testing (QuickCheck)
--        - Be a responsible citizen and use magical typeclasses, once they come out
--        - Cardinal direction lenses (?)
--        - Simplify signatures where possible (ConstraintKinds?)
--        - Deal with corner-cases
--        - Loosen constraints where possible (eg. width, although it'd require a slightly less elegant impl.)
--        - Reconsider the Applicative constraint (see linear, should we use Additive instead)

-- GHC Pragrams ----------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

-- API -------------------------------------------------------------------------

module Data.AABB where

-- We'll need these ------------------------------------------------------------

import Control.Applicative (liftA2)
import Data.List           (sort)
import Data.Traversable
import Lens.Micro
import Linear.V1           (R1, _x)
import Linear.V2           (R2, _y)
import Linear.V3           (R3, _z)
import Linear.V4           (R4, _w)

-- Definitions -----------------------------------------------------------------

-- | The protagonist in our Cartesian fable
-- TODO | - I kinda want to drop the 'f', but uncle Ed says not to :'(
data AABB f a = AABB !(f a) !(f a) deriving (Eq, Show)

-- Constructors ----------------------------------------------------------------

-- |
-- TODO | - Rename (?)
fromCornerSize :: (Applicative f, Num a) => f a -> f a -> AABB f a
fromCornerSize c sz = AABB c (liftA2 (+) c sz)

-- Lenses ----------------------------------------------------------------------

-- | Focus on the first field, presumably the lowest value for each dimension
lo :: Lens' (AABB f a) (f a)
lo f (AABB a b) = (\new -> AABB new b) <$> f a


-- | Focus on the second field, presumably the highest value for each dimension
hi :: Lens' (AABB f a) (f a)
hi f (AABB a b) = (\new -> AABB a new) <$> f b


-- |
axes :: Applicative f => Lens' (AABB f a) (f (a, a))
axes f (AABB a b) = uncurry AABB . unzipA <$> f (zipA a b)


-- | Creates a 'Lens' that focuses on a single axis, given another 'Lens' (presumably, 'x', 'y' or 'z')
axis :: Applicative f => Lens' (f (a, a)) (a, a) -> Lens' (AABB f a) (a, a)
axis = (axes .)


-- |
size :: (Applicative f, Num a) => Lens' (AABB f a) (f a)
size f (AABB a b) = AABB a <$> (liftA2 (+) a <$> f (liftA2 (-) b a))


-- |
width :: (Applicative f, R1 f, Num a) => Lens' (AABB f a)  a
width = size.x


-- |
height :: (Applicative f, R2 f, Num a) => Lens' (AABB f a) a
height = size.y


-- |
depth :: (Applicative f, R3 f, Num a) => Lens' (AABB f a)  a
depth = size.z


-- |
--volume,area,surfaceArea,circumference


-- |
--pinned :: _
--centre ::
--centre = pinned $ pure _

--------------------------------------------------------------------------------

-- |
--normalise :: Ord a => AABB f a -> AABB f a
--normalise


-- |
-- TODO | - Refactor (cf. traverse)
intersect :: (Applicative f, Traversable f, Ord a) => AABB f a -> AABB f a -> Maybe (AABB f a)
intersect a b = fmap (uncurry AABB . unzipA) . sequenceA $ liftA2 overlap (a^.axes) (b^.axes)


-- | Finds the overlap between two inclusive ranges
-- TODO | - Polymorphic intervals (?)
--        - Reuse existing type (?)
--        - Simplify, refactor
overlap :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
overlap a b
  | min a b /= (lo'', lo')  = Just (lo', hi')
  | otherwise               = Nothing
  where
    [lo'', lo', hi', _] = sort [fst a, snd a, fst b, snd b]


-- |
-- TODO | - Reuse pre-existing version of 'between' (maybe intervals has one?)
inside :: (Applicative f, Foldable f, Ord a) => f a -> AABB f a -> Bool
inside p (AABB lo' hi') = and $ between <$> p <*> lo' <*> hi'
  where
    between n a b = (a <= n) && (n <= b)


-- |
--vertices :: _
--faces :: _
--edges :: _

--------------------------------------------------------------------------------

-- |
x :: R1 f => Lens' (f a) a
x = _x


-- |
y :: R2 f => Lens' (f a) a
y = _y


-- |
z :: R3 f => Lens' (f a) a
z = _z


-- |
w :: R4 f => Lens' (f a) a
w = _w

--------------------------------------------------------------------------------

-- | Zip two 'Applicative's
zipA :: Applicative f => f a -> f a -> f (a, a)
zipA = liftA2 (,)


-- | Unzip an 'Applicative'
unzipA :: Applicative f => f (a, a) -> (f a, f a)
unzipA a = (fst <$> a, snd <$> a)
