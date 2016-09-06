{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Axis
  (bottomAxis
  ,leftAxis
  ,rightAxis
  ,topAxis)
  where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude             hiding (dot, frame)
import Prelude                      hiding (Left, Right)
--
import Scale

axisHorizontally
  :: (Scale scale)
  => scale -> QDiagram B V2 Double Any
axisHorizontally axisScale =
  position ([(p2 (0,0),axisLine l)
            ,(p2 (-(l / 2),0),tickEnds) -- for start tick
            ,(p2 (l / 2,0),tickEnds) -- for end tick
             ] <>
            map (\tp -> (p2 (tp - (l / 2),0),tick)) tickPositions)
  where tickEnds = (showOrigin . lineWidth veryThin . vrule) (0.1 * l)
        tick = (showOrigin . lineWidth veryThin . vrule) (0.05 * l)
        tickPositions =
          [(minRange axisScale),(minRange axisScale) + tickStep .. (maxRange axisScale)]
        tickStep = l / 10
        l = maxRange axisScale - minRange axisScale

type Length = Double

axisLine :: Length -> QDiagram B V2 Double Any
-- xAxis = (showOrigin . lineWidth veryThin . fromVertices) [p2 (0,0),p2 (chartWidth,0)]
axisLine = (showOrigin . lineWidth veryThin . hrule)

bottomAxis, leftAxis, rightAxis, topAxis
  :: (Scale scale)
  => scale -> QDiagram B V2 Double Any
bottomAxis = showOrigin . axisHorizontally

leftAxis = showOrigin . rotate (90 @@ deg) . axisHorizontally

rightAxis = showOrigin . rotate (-90 @@ deg) . axisHorizontally

topAxis = showOrigin . rotate (180 @@ deg) . axisHorizontally
