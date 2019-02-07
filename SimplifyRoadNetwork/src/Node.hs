{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}

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

data NodeCsv =
  NodeCsv
    Node
    Longitude
    Latitude
    SignalOut
  deriving Show

instance FromNamedRecord NodeCsv where
  parseNamedRecord m =
    NodeCsv
      <$> m .: "node_id"
      <*> m .: "longitude"
      <*> m .: "latitude"
      <*> m .: "signal"

decodeNodeCsv :: FilePath -> IO Nodes
decodeNodeCsv fp = trace "decodeNodeCsv" $ do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsv)
  return $ makeNodes ls

-- data NodeCond = NodeCond { latitude :: Latitude, longitude :: Longitude, signalOut :: SignalOut } deriving (Eq, Show)

-- type NodeCsv = Map.Map Node NodeCond

data Coordinates =
  Coordinates
    { longitude :: Longitude
    , latitude :: Latitude
    }
  deriving (Eq, Show, Ord)

type Node = Int
type Signal = Bool
data NodeCond = NodeCond { coordinates :: Coordinates, signal :: Signal }

type Nodes = Map.Map Node NodeCond

{-
makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut n lat lon signalOut) = Map.insert n (NodeCond lat lon signalOut)
-}

makeNodes :: V.Vector NodeCsv -> Nodes
makeNodes nco =
  foldr (\(NodeCsv n lon lat so) ns -> Map.insert n (NodeCond (Coordinates lon lat) $ f so) ns) Map.empty nco
  where
    f (Just "yes") = True
    f _ = False

encodeNodes :: Nodes -> String
encodeNodes nc = 
  "node,longitude,latitude,signal"
    <> Map.foldrWithKey
      ( \n (NodeCond (Coordinates lon lat) s) str ->
          str
            <> "\n"
            <> show n <> ","
            <> show lon <> ","
            <> show lat <> ","
            <> show s
      )
    "" nc
