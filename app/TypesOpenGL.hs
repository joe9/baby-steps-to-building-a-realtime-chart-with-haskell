{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module TypesOpenGL where

import Linear.V2

type Height = Double
type Width = Double
type Bid = Double
type Ask = Double
type Volume = Double

type X = Double
type Y = Double

--   -- https://hackage.haskell.org/package/colour-2.3.3
--   -- https://hackage.haskell.org/package/prizm-0.3.1.2
-- useRGB :: RGB Double -> Color
-- useRGB (RGB r b g) = makeColor (realToFrac r) (realToFrac b) (realToFrac g) 1

--   -- http://www.w3.org/TR/SVG11/types.html#ColorKeywords
-- useColor :: Colour Double -> Color
-- useColor = useRGB . toSRGB

-- dot :: V2 Double -> Picture
-- dot (V2 x y) =
--   (color black . translate (realToFrac x) (realToFrac y) . circleSolid) 1

-- v2ToPoint :: V2 Double -> Point
-- v2ToPoint (V2 x y) = (realToFrac x, realToFrac y)

rf :: Double -> Float
rf = realToFrac
