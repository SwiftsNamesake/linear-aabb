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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
import           Data.AABB

-- Definitions -----------------------------------------------------------------

data App = App {
  fInput :: Input App
} deriving (Show)

(~>) = (^.)

window :: SimpleGetter App GLFW.Window
window = to (fWindow . fInput)

--window :: SimpleGetter App GLFW.Window
--window = to fWindow

--------------------------------------------------------------------------------

-- |
setupGraphics :: App -> IO ()
setupGraphics app = do
  -- Shaders
  vs <- GL.createShader GL.VertexShader
  fs <- GL.createShader GL.FragmentShader

  GL.shaderSourceBS vs $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 130"
      , ""
      ])

  GL.shaderSourceBS fs $= Text.encodeUtf8
      (Text.pack $ unlines
        [ "#version 130"
        , ""
        ])

  GL.compileShader vs
  GL.compileShader fs

  pr <- GL.createProgram
  GL.attachShader pr vs
  GL.attachShader pr fs

  -- _ <- GL.uniformLocation ____

  -- GL.attribLocation "___" $= ___

  GL.linkProgram pr
  GL.currentProgram $= Just pr

  -- Vertices


-- |
run :: EitherT String IO App
run = setup >>= loop


-- |
loop :: App -> EitherT String IO App
loop = iterateWhileM (\app -> not <$> shouldClose (app~>window)) $ \app -> do
  lift . GLFW.swapBuffers $ app~>window
  lift $ render app
  lift $ GLFW.pollEvents
  advance app


-- |
advance :: App -> EitherT String IO App
advance old = return old


-- |
render :: App -> IO ()
render app = do
  GL.viewport $= (Position 0 0, Size 100 100) -- TODO: Size
  GL.clear [GL.ColorBuffer]


-- |
shouldClose :: MonadTrans t => GLFW.Window -> t IO Bool
shouldClose = lift . GLFW.windowShouldClose


-- |
main :: IO ()
main = either print (\_ -> putStrLn "Everything went according to plan") =<< runEitherT run
