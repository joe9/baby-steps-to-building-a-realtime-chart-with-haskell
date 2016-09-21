{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TypesOpenGL where

import Data.Colour.SRGB
import Data.Tuple.Select
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Linear.V2
import Data.Int
--
import GLFWStuff
import OpenGLStuff
import ScaleDataUnboxedVector

type Height = Double

type Width = Double

type Bid = Double

type Ask = Double

type Volume = Double

-- <joe9> Is Integer = Int64? I want to use an Integer in Vector.Unboxed value. But, Vector.Unboxed does not have an instance for Integer, just for Int64
-- <ClaudiusMaximus> Integer is arbitrary precision, so variable size, so can't be unboxed
-- <joe9> can I represent the seconds time from getPOSIXTime in Int64?
-- <joe9> I am trying to figure out how to store a time in microseconds in an unboxed vector. Int64 good enough?
-- <ClaudiusMaximus> > 1970 + floor (fromIntegral (maxBound :: Int64) / (1e6 * 60 * 60 * 24 * 365)) -- joe9, this should be the year you could store microseconds until, unless i messed up somewhere
-- <lambdabot>  294441
-- <joe9> ClaudiusMaximus: That is way cool.
type AsOf = Int64

type PriceData = (Bid, Ask, Volume, AsOf)

type X = Double

type Y = Double

data Value
    = ValueCursorPosition Double
                          Double
    | ValueDimensions Width
                      Height
    | ValueInt Int
    | ValueAsOf Int64
    | ValueInteger Integer
    | ValueEmpty
    deriving Eq

data DrawableType
    = Screen
    | Frame
    | PriceChart
    | VolumeChart
    | HorizontalCrosshair
    | VerticalCrosshair
    deriving Show

data Drawable = Drawable
    { dPreviousValue :: Maybe Value
    , dCurrentValue :: State -> VU.Vector PriceData -> Value
    , dLoadBufferAndBuildDrawFunction :: State -> VU.Vector PriceData -> Scale -> Scale -> Scale -> Drawable -> IO (IO ())
    , dDraw :: IO ()
    , dVertexArrayId :: VertexArrayId
    , dBufferId :: BufferId
    , dColour :: Colour Double
    , dTransparency :: Maybe Double
    , dType :: DrawableType
    }

bid :: PriceData -> Bid
bid = sel1

ask :: PriceData -> Bid
ask = sel2

volume :: PriceData -> Bid
volume = sel3

asof :: PriceData -> AsOf
asof = sel4

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
v2ToVertex
    :: V2 Double -> VS.Vector Float
v2ToVertex (V2 x y) = VS.fromList [realToFrac x, realToFrac y]

rf :: Double -> Float
rf = realToFrac
