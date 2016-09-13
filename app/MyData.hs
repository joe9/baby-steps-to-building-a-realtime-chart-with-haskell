{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module MyData where

import           Control.Monad
import           System.Random
import           Data.List
import qualified Data.IntMap.Strict                  as IntMap

data MyData =
  MyData {mdId     :: Int
         ,mdBid    :: Double
         ,mdAsk    :: Double
         ,mdVolume :: Double}
  deriving (Eq,Read,Show)

staticDataSeries :: IntMap.IntMap MyData
staticDataSeries =
  (IntMap.fromList . map (\d -> (mdId d,d)))
    [MyData 1 1.19 1.26 1000
    ,MyData 2 1.22 1.27 2000
    ,MyData 3 1.27 1.37 1000
    ,MyData 4 1.37 1.47 0
    ,MyData 5 1.67 1.97 3000
    ,MyData 6 1.57 1.67 1000
    ,MyData 7 1.47 1.57 1000
    ,MyData 8 1.27 1.37 500
    ,MyData 9 1.17 1.25 5000
    ,MyData 10 1.1 1.15 0]

buildDataSeries :: IO ( IntMap.IntMap MyData)
buildDataSeries = do
  -- with 10000 elements, cairo and gloss take 16 seconds to render
  -- with 1000 elements, cairo and gloss are instantaneous
  let numberOfElements = 10000
  bids <- replicateM numberOfElements (randomRIO (1,2))
  asks <- replicateM numberOfElements (randomRIO (2,3))
  volumes <- replicateM numberOfElements (randomRIO (0,1000000))
  (return . IntMap.fromList . map (\d -> (mdId d,d))) (zipWith4 MyData [1..] bids asks volumes)

-- below functions for building C test data
showBidAndAskVertices :: IntMap.IntMap MyData -> String
showBidAndAskVertices mds = concatMap showBidAndAskVertex mds

showBidAndAskVertex :: MyData -> String
showBidAndAskVertex md = " " ++ show (mdBid md - 2) ++ "f," ++ show (mdAsk md - 2) ++ "f,\n"
