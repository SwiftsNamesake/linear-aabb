-- |
-- Module      : Main
-- Description :
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | -
--        -

-- GHC Pragmas -----------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

-- *
--import           Graphics.DrawingCombinators  as Draw
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Control.Monad.Trans.Either
import           Data.Set                      as Set
import           Graphics.Rasterific
import           Graphics.Rendering.OpenGL     (($=))
import           Graphics.Rendering.OpenGL     as GL
-- import qualified Graphics.Rendering.OpenGL.Raw as GL
import           Graphics.UI.GLFW              as GLFW
import           Lens.Micro
import           Linear

-- * 
import Graphics.UIKit

-- *
import           Data.AABB


-- Definitions -----------------------------------------------------------------

data App = App {
  fInput :: Input App
} deriving (Show)

(~>) = (^.)

window :: SimpleGetter App GLFW.Window
window = to (fWindow . fInput)

--------------------------------------------------------------------------------
