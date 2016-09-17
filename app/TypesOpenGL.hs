{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module TypesOpenGL where

import Linear.V2
import qualified Data.Vector.Storable as VS
import Data.Tuple.Select

type Height = Double
type Width = Double
type Bid = Double
type Ask = Double
type Volume = Double
type PriceData = (Bid,Ask,Volume)

type X = Double
type Y = Double

bid :: PriceData -> Bid
bid = sel1

ask :: PriceData -> Bid
ask = sel2

volume :: PriceData -> Bid
volume = sel3

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

v2ToVertex :: V2 Double -> VS.Vector Float
v2ToVertex (V2 x y) = VS.fromList [realToFrac x, realToFrac y]

rf :: Double -> Float
rf = realToFrac
