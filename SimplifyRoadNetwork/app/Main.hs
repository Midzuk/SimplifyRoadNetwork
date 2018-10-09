--{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
module Main where

import           Algorithm.Search
import           Csv.LinkCsv          (decodeLinkCsv, makeLinkCsv, encodeLinkCsv, LinkWithCond)
import           Csv.NodeCsv          (decodeNodeCsv, makeNodeCsv, encodeNodeCsv)
import qualified Data.Map.Lazy        as Map
import qualified Data.Set             as Set
import           Network
import           System.IO.Unsafe
import           Csv.NetworkCsv (NetworkCsv(..), simplifyNetworkCsv, makeNetwork)
import qualified System.Directory as Dir
import Data.List (find)
import System.Environment (getArgs)
import Data.Vector as V (filter, Vector)
import qualified Data.Text            as T

main :: IO ()
main = do
  --args <- getArgs
  --let [latOrg, lonOrg, latDest, lonDest] = read <$> args

  lc <- decodeLinkCsv "/temporary/temp_links.csv"
  nc <- decodeNodeCsv "/temporary/temp_nodes.csv"
 
  --let lc1 = V.filter (\(_, Just _highway) -> _highway /= "footway" && _highway /= "service") lc
  let nwc = NetworkCsv lc nc
  let snwc@(NetworkCsv slc snc) = simplifyNetworkCsv nwc

  cd <- Dir.getCurrentDirectory
  writeFile (cd <> "/output/simple_link.csv") $ encodeLinkCsv slc
  writeFile (cd <> "/output/simple_node.csv") $ encodeNodeCsv snc
