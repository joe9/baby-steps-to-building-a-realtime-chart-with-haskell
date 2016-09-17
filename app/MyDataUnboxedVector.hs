
module MyDataUnboxedVector where

import           Control.Monad
import           System.Random
import           Data.List
import qualified Data.Vector.Unboxed                  as VU
import Prelude
import TypesOpenGL

-- data MyData =
--   MyData {mdId     :: Int
--          ,mdBid    :: Double
--          ,mdAsk    :: Double
--          ,mdVolume :: Double}
--   deriving (Eq,Read,Show)

-- understand the need for unboxed vector
-- http://stackoverflow.com/questions/34692809/lists-boxed-vectors-and-unboxed-vectors-for-heavy-scientific-computations
-- if the data type becomes more complicated and I need to derive an
-- Unbox instance
-- http://stackoverflow.com/questions/22882228/how-to-store-a-haskell-data-type-in-an-unboxed-vector-in-continuous-memory
staticDataSeries :: VU.Vector PriceData
staticDataSeries =
  VU.fromList
     [(1.19,1.26,1000)
     ,(1.22,1.27,2000)
     ,(1.27,1.37,1000)
     ,(1.37,1.47,0)
     ,(1.67,1.97,3000)
     ,(1.57,1.67,1000)
     ,(1.47,1.57,1000)
     ,(1.27,1.37,500)
     ,(1.17,1.25,5000)
     ,(1.1,1.15,6000)]

buildDataSeries :: IO (VU.Vector PriceData)
buildDataSeries = do
  -- with 10000 elements, cairo and gloss take 16 seconds to render
  --                      OpenGL is taking around 10 seconds to render
  -- with 1000 elements, cairo and gloss are instantaneous
  let numberOfElements = 10
  bids <- VU.replicateM numberOfElements (randomRIO (1,2))
  asks <- VU.replicateM numberOfElements (randomRIO (2,3))
  volumes <- VU.replicateM numberOfElements (randomRIO (0,1000000))
  return (VU.zip3 bids asks volumes)
