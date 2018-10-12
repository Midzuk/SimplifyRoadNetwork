{-# LANGUAGE TemplateHaskell #-}
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

type Latitude = Double
type Longitude = Double
type SignalOut = Maybe T.Text

data NodeCsvOut = NodeCsvOut Node Latitude Longitude SignalOut deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "node_id"
      <*> m .: "lat"
      <*> m .: "lon"
      <*> m .: "signal"

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls

data NodeCond = NodeCond { latitude :: Latitude, longitude :: Longitude, signalOut :: SignalOut } deriving (Eq, Show)

type NodeCsv = Map.Map Node NodeCond

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n lat lon signalOut) = Map.insert n (NodeCond lat lon signalOut)

encodeNodeCsv :: NodeCsv -> String
encodeNodeCsv nc = 
  "node_id,lat,lon"
    <> Map.foldrWithKey
      (\node (NodeCond lat lon _) str ->
        str
          <> "\n"
          <> show node <> ","
          <> show lat <> ","
          <> show lon)
    "" nc
