{-# LANGUAGE OverloadedStrings #-}

module Node where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir

type Node = Int

type Lat = Double
type Lon = Double

-- とりあえず信号機は無視
data NodeCsvOut = NodeCsvOut Node Lat Lon deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "node_id"
      <*> m .: "lat"
      <*> m .: "lon"

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls


type NodeCsv =
  Map.Map Node (Lat, Lon)

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n lat lon) = Map.insert n (lat, lon)

encodeNodeCsv :: NodeCsv -> String
encodeNodeCsv nc = 
  "node_id,lat,lon"
    <> Map.foldrWithKey
      (\node_ (lat_, lon_) str_ -> str_ <> "\n" <> show node_ <> "," <> show lat_ <> "," <> show lon_)
        "" nc
