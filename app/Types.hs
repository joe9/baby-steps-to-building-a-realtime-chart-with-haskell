{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

import Graphics.Rendering.Cairo        hiding (scale,x,y)
import Data.Colour.SRGB
import Data.Colour.Names
import Linear.V2

type Height = Double
type Width = Double
type Bid = Double
type Ask = Double
type Volume = Double

type X = Double
type Y = Double

  -- https://hackage.haskell.org/package/colour-2.3.3
  -- https://hackage.haskell.org/package/prizm-0.3.1.2
useRGB :: RGB Double -> Render ()
useRGB (RGB r b g) = setSourceRGB r b g

  -- http://www.w3.org/TR/SVG11/types.html#ColorKeywords
useColor :: Colour Double -> Render ()
useColor = useRGB . toSRGB

dot :: V2 Double -> Render ()
dot (V2 x y) = do
  useColor black
  arc x y 1 0 (2 * pi) -- 0.07
  fill
