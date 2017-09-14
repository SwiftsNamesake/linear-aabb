-- |
-- Module      : Data.AABB
-- Description : Defines types and operations on axis-aligned bounding boxes
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : ?
-- Portability : ?
--

-- TODO | -
--        -

-- API -------------------------------------------------------------------------

module Data.AABB where

-- We'll need these ------------------------------------------------------------

-- Definitions ----------------------------------------------------------------

-- | The protagonist in our Cartesian fable
-- TODO | - I kinda want to drop the 'f', but uncle Ed says not to :'(
data AABB f a = AABB !(f a) !(f a) deriving (Eq, Show)
