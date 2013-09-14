-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Control.Monad (forM_)
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL hiding (Program, color)
import Graphics.UI.GLUT hiding (Program, color)
import System.Exit
import Turtle

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]

-- | ...
runGraphical :: Program -> IO ()
runGraphical program = do
  (_, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= (display program newTurtle)
  mainLoop

display :: Program -> Turtle -> IO ()
display prog init = do
  clear [ColorBuffer]
  forM_ (run prog init) draw
  flush
  where draw (t1, t2) =
          if (pen t1) then
            renderPrimitive Lines $ do
              OpenGL.color $ Color3 r g b
              OpenGL.vertex $ Vertex3 x1 y1 0
              OpenGL.vertex $ Vertex3 x2 y2 0
          else return ()
          where (x1, y1) = pos t1
                (x2, y2) = pos t2
                (r, g, b) = col t1


-- display = do
--   clear [ColorBuffer]
--   renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) myPoints
--   flush


{- -- Alernatively using GTK

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

runGraphical :: a -> IO ()
runGraphical program = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 30 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window
     drawin <- widgetGetDrawWindow canvas
     onExpose canvas (\x -> do renderWithDrawable drawin myDraw
                               return True)

     onDestroy window mainQuit
     mainGUI

myDraw :: Render ()
myDraw = do
    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath

    stroke

-}
