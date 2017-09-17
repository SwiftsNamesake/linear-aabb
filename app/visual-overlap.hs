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

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

-- *
--import           Graphics.DrawingCombinators  as Draw
import           Control.Applicative
import           Control.Concurrent.STM.TChan
import           Control.Exception.Safe
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Loops
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Either
import           Data.Strict.Set              as Set
import           Graphics.Rasterific
import           Graphics.Rendering.OpenGL    as GL
import           Graphics.UI.GLFW             as GLFW
import           Lens.Micro
import           Linear

-- *
import           Data.AABB

-- Definitions -----------------------------------------------------------------

data App = App {
  fInput  :: Input,
  fWindow :: GLFW.Window
}

data Input = Input {
  fMessages   :: TChan Message,
  fTime       :: Double,
  fWindowRect :: AABB V2 Int,
  fCursor     :: V2 Double,
  fKeyboard   :: Keyboard
}

type Keyboard = Set GLFW.Key

(~>) = (^.)

input :: SimpleGetter App ()
input = to fInput

window :: SimpleGetter App GLFW.Window
window = to fWindow

--------------------------------------------------------------------------------

-- | Promote a 'Bool' to an 'EitherT'
insist :: Monad m => Bool -> e -> EitherT e m ()
insist False e = left e
insist True  _ = right ()


-- | Promote a 'Maybe' to an 'EitherT'
-- TODO | - Factor out
explain :: Monad m => e -> Maybe a -> EitherT e m a
explain e = maybe (left e) right


-- | Do nothing whatsoever
pass :: Applicative f => f ()
pass = pure ()

--------------------------------------------------------------------------------

-- |
setup :: EitherT String IO App
setup = do
  b <- lift GLFW.init
  insist b "Failed to initialise GLFW"
  win <- explain "Failed to create window" =<< lift (GLFW.createWindow dx dy "Visual Overlap" Nothing Nothing)
  lift . GLFW.makeContextCurrent $ Just win
  return $ App { fInput = (), fWindow = win }
  where
    (V2 dx dy) = V2 720 480


-- |
run :: EitherT String IO ()
run = do
  app <- setup
  iterateUntilM (\_ -> lift . GLFW.windowShouldClose $ app~>window) $ \app -> do
    --render app
    lift . GLFW.swapBuffers $ app~>window
    lift $ render
    lift $ GLFW.pollEvents
    advance app
  pass


-- |
advance :: EitherT String IO App
advance old = _


-- |
--render :: _

-- |
main :: IO ()
main = either print (\_ -> putStrLn "Everything went according to plan") =<< runEitherT run
