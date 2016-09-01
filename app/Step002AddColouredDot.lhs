> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}

> module Main where
>
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude

Draw a single dot.

> dot :: Diagram B
> dot = (fc blue . circle) 1

compile using similar commands as in Step 1.

> main :: IO ()
> main = mainWith (Main.dot :: Diagram B)
