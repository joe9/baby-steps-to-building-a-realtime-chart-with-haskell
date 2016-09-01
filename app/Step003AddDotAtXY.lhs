> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}

> module Main where
>
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude

Draw a single blue coloured dot at x = 20 and y = 10. Also, show the
  local origin.

> dot :: Diagram B
> dot = (translateY 20 . translateX 10 . showOrigin . fc blue . circle) 1

compile using similar commands as in Step 1.

> main :: IO ()
> main = mainWith (Main.dot :: Diagram B)
