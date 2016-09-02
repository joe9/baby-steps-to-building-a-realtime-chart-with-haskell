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
> dot = circle 1

TMPDIR=/tmp/ghc stack build
TMPDIR=/tmp/ghc stack exec Step001AddDot -- --help
TMPDIR=/tmp/ghc stack exec Step001AddDot -- --width 500 --height 500 --output Step001AddDot.svg

> main :: IO ()
> main = mainWith (Main.dot :: Diagram B)
