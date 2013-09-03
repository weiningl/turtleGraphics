-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import System.Exit
import Turtle

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]

-- | ...
runGraphical :: Program -> IO ()
runGraphical program = do
  (_, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= (display program [newTurtle])
  mainLoop

display :: Program -> [Turtle] -> IO ()
display prog initPos = do
  clear [ColorBuffer]
  pos <- return initPos
  prog pos
  flush

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
