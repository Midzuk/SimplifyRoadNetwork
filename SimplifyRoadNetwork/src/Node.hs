-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Node where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir
import qualified Data.Set             as Set
import Debug.Trace



-- lon lat
type Longitude = Double
type Latitude = Double
type SignalOut = Maybe T.Text

data NodeCsvOut = NodeCsvOut NodeId Longitude Latitude SignalOut deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "node_id"
      <*> m .: "longitude"
      <*> m .: "latitude"
      <*> m .: "signal"

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = trace "decodeNodeCsv" $ do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls

-- data NodeCond = NodeCond { latitude :: Latitude, longitude :: Longitude, signalOut :: SignalOut } deriving (Eq, Show)

-- type NodeCsv = Map.Map Node NodeCond

data Coordinates = Coordinates { latitude :: Latitude, longitude :: Longitude }
  deriving (Eq, Show, Ord)

type NodeId = Int
type Signal = Bool
data NodeCond = NodeCond { coordinates :: Coordinates, signal :: Signal }

-- type Node = Int
-- type NodeId = Int
-- data Node = Node { nodeId :: NodeId, coordinates :: Coordinates, signalOut :: SignalOut } deriving (Eq, Ord, Show)

type NodeCsv = Set.Set Node

{-
makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n lat lon signalOut) = Map.insert n (NodeCond lat lon signalOut)
-}

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv =
  foldr ((\(NodeCsvOut ni lat lon so) _ns -> (`Set.insert` _ns) $ Node { nodeId = ni, coordinates = Coordinates lat lon, signalOut = so })) Set.empty

encodeNodeCsv :: NodeCsv -> String
encodeNodeCsv nc = 
  "node_id,latitude,longitude"
    <> Set.foldr
      (\(Node ni (Coordinates lat lon) _) str ->
        str
          <> "\n"
          <> show ni <> ","
          <> show lat <> ","
          <> show lon)
    "" nc
