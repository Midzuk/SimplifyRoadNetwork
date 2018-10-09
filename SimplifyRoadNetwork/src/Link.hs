{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ViewPatterns #-}

module Link where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Lazy        as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Network              (Graph (..), Link (..), OD (..), composeLink, compose, showMaybe)
import qualified System.Directory     as Dir

type Node = Int

type Org = Node
type Dest = Node

type Dist = Double --距離
type Highway = Maybe T.Text
type Oneway = Maybe T.Text
type Bridge = Maybe T.Text
type Width = Maybe T.Text

--data LinkCsvOut = LinkCsvOut Org Dest Dist Highway Oneway Bridge Width deriving (Show)
data LinkCsvOut = LinkCsvOut Org Dest Dist Highway Oneway deriving (Show)

instance FromNamedRecord LinkCsvOut where
  parseNamedRecord m =
    LinkCsvOut
      <$> m .: "node_id_org"
      <*> m .: "node_id_dest"
      <*> m .: "distance"
      <*> m .: "highway"
      <*> m .: "oneway"
      -- <*> m .: "bridge"
      -- <*> m .: "width"



decodeLinkCsv :: FilePath -> IO LinkCsv
decodeLinkCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector LinkCsvOut)
  return $ makeLinkCsv ls

type LinkCond = Highway -- (Highway, Bridge, Width)

type LinkWithCond = (Link, LinkCond)

type LinkCsv =
  V.Vector LinkWithCond
  --Map.Map OD (Dist, LinkCond)

makeLinkCsv :: V.Vector LinkCsvOut -> LinkCsv
makeLinkCsv = foldr f []
  where
    f (LinkCsvOut org dest dist highway oneway) -- bridge width)
      | oneway == Just "yes" = V.cons linkOD
      | oneway == Just "-1" = V.cons linkDO
      | otherwise = V.cons linkOD . V.cons linkDO
      where
        linkOD = (Link (Edge (org :->: dest)) dist, highway) -- (highway, bridge, width))
        linkDO = (Link (Edge (dest :->: org)) dist, highway) -- (highway, bridge, width))

encodeLinkCsv :: LinkCsv -> String
encodeLinkCsv lc =
  "node_id_org,node_id_dest,distance,highway" -- ,Bridge,Width"
    <> foldr
      (\(Link g dist, highway_) str_ ->
        let
          org_ :->: dest_ = compose g
        in 
          str_ <> "\n" <> show org_ <> "," <> show dest_ <> "," <> show dist <> "," <> showMaybe highway_) -- <> "," <> showMaybe bridge_ <> "," <> showMaybe width_)
            "" lc

{-
makeLinkCsvMap :: V.Vector LinkCsv -> LinkCsvMap
makeLinkCsvMap ls = V.foldr f Map.empty ls
  where
    f (LinkCsv org dest dist highway oneway bridge width) =
      Map.insert (org :->: dest) (dist, (highway, oneway, bridge, width))
-}
