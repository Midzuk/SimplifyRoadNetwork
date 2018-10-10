{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Link where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Lazy        as Map
import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir
import           Data.Monoid

import           Node




type Cost = Double

data Link = Node :->: Node deriving (Eq, Show)
infixr 5 :->:

instance Ord Link where
  compare l1@(n1 :->: n2) l2@(n3 :->: n4)
    | l1 == l2 = EQ
    | otherwise =
      case compare n1 n3 of
        EQ ->
          if n2 < n4 then LT else GT
        o -> o

instance Semigroup Link where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup Link Error."

data Path = Path Graph Cost deriving Show

instance Eq Path where
  Path g1 c1 == Path g2 c2 = g1 == g2 && c1 == c2

instance Ord Path where
  compare l1@(Path g1 c1) l2@(Path g2 c2)
    | l1 == l2 = EQ
    | c1 == c2 =
      if g1 < g2 then LT else GT
    | c1 < c2 = LT
    | otherwise = GT

instance Semigroup Path where
  Path g1 c1 <> Path g2 c2 = Path (g1 <> g2) (c1 + c2)

--type Network = Map.Map Link Path

data Graph = Edge Link | Graph (V.Vector Link) deriving Show

compose :: Graph -> Link
compose (Edge l) = l
compose (Graph v) = foldr1 (<>) v

composePath :: Path -> Link
composePath (Path g _) = compose g

instance Eq Graph where
  g1 == g2 = compose g1 == compose g2

instance Ord Graph where
  compare g1 g2
    | g1 == g2 = EQ
    | compose g1 < compose g2 = LT
    | otherwise = GT

instance Semigroup Graph where
  Edge link1 <> Edge link2 = Graph [link1, link2]
  Edge l <> Graph v = Graph $ V.cons l v
  Graph v <> Edge l = Graph $ V.snoc v l
  Graph v1 <> Graph v2 = Graph $ v1 <> v2

{-
networkFromList :: [(Link, Cost)] -> Network
networkFromList odcs = Map.fromList $ (\(l, c) -> (l, Path (Edge l) c)) <$> odcs

insertLink :: Path -> Network -> Network
insertLink l@(Path (compose -> l) _) n =
  case n Map.!? l of
    Nothing -> Map.insert l l n
    Just _  -> n
-}

isNextPath :: Path -> Path -> Bool
Path g2 _ `isNextPath` Path g1 _ = g2 `isNextGraph` g1

isNextGraph :: Graph -> Graph -> Bool
Edge l2 `isNextGraph` Edge l1 = l2 `isNextLink` l1
Edge l `isNextGraph` g@(Graph v) = (l `isNextLink` compose g) && notElem (invertLink l) v -- && V.notElem Link v
g@(Graph v) `isNextGraph` Edge l = (compose g `isNextLink` l) && notElem (invertLink l) v -- && V.notElem Link v
g2@(Graph v2) `isNextGraph` g1@(Graph v1) = (compose g2 `isNextLink` compose g1) && not (any (`elem` v1) v2) && not (any (`elem` v1) (invertLink <$> v2))

isNextLink :: Link -> Link -> Bool
(n3 :->: n4) `isNextLink` (n1 :->: n2) = n2 == n3 && n1 /= n4

{-
inverseNetwork :: Network -> Network
inverseNetwork = Map.foldrWithKey (\(n1 :->: n2) (Path _ c) n -> Map.insert (n2 :->: n1) (Path (Edge (n2 :->: n1)) c) n) Map.empty
-}

invertLink :: Link -> Link
invertLink (n1 :->: n2) = n2 :->: n1

overlap :: Graph -> Graph -> Bool
overlap (Edge l1) (Edge l2) = l1 == l2
overlap (Edge l) g@(Graph v) = l `elem` v
overlap g@(Graph v) (Edge l) = l `elem` v
overlap g1@(Graph v1) g2@(Graph v2) = any (`elem` v1) v2

overlapLink :: Path -> Path -> Bool
overlapLink (Path g1 _) (Path g2 _) = overlap g1 g2

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""



type Org = Node
type Dest = Node

type Dist = Double --距離
type Highway = Maybe T.Text
type Oneway = Maybe T.Text
type MaxSpeed = Maybe T.Text
type Lanes = Maybe T.Text
type Width = Maybe T.Text
type Bridge = Maybe T.Text
type Tunnel = Maybe T.Text
type Surface = Maybe T.Text
type Service = Maybe T.Text
type Foot = Maybe T.Text
type Bicycle = Maybe T.Text

type Signal = Sum Int

data LinkCsvOut = LinkCsvOut Org Dest Dist Highway Oneway MaxSpeed Lanes Width Bridge Tunnel Surface Service Foot Bicycle deriving (Show)

instance FromNamedRecord LinkCsvOut where
  parseNamedRecord m =
    LinkCsvOut
      <$> m .: "node_id_org"
      <*> m .: "node_id_dest"
      <*> m .: "distance"
      <*> m .: "highway"
      <*> m .: "oneway"
      <*> m .: "max_speed"
      <*> m .: "lanes"
      <*> m .: "width"
      <*> m .: "bridge"
      <*> m .: "tunnel"
      <*> m .: "surface"
      <*> m .: "service"
      <*> m .: "foot"
      <*> m .: "bicycle"

--dist, highway, oneway, max_speed, lanes, width, bridge, tunnel, surface, service, foot, bicycle

decodeLinkCsv :: FilePath -> NodeCsv -> IO LinkCsv
decodeLinkCsv fp nc = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector LinkCsvOut)
  return $ makeLinkCsv nc ls

data LinkCond = LinkCond Highway MaxSpeed Lanes Width Bridge Tunnel Surface Service Foot Bicycle deriving (Eq, Show)

data LinkWithCond = LinkWithCond { path :: Path, linkCond :: LinkCond, signal :: Signal } deriving (Eq, Show)

type LinkCsv = V.Vector LinkWithCond

makeLinkCsv :: NodeCsv -> V.Vector LinkCsvOut -> LinkCsv
makeLinkCsv nc = foldr f []
  where
    f (LinkCsvOut org dest dist highway oneway max_speed lanes width bridge tunnel surface service foot bicycle)
      | oneway == Just "yes" = V.cons linkOD
      | oneway == Just "-1" = V.cons linkDO
      | otherwise = V.cons linkOD . V.cons linkDO
      where
        linkOD = LinkWithCond (Path (Edge (org :->: dest)) dist) (LinkCond highway max_speed lanes width bridge tunnel surface service foot bicycle) $ f dest
        linkDO = LinkWithCond (Path (Edge (dest :->: org)) dist) (LinkCond highway max_speed lanes width bridge tunnel surface service foot bicycle) $ f org

        f n =
          case nc Map.! n of
            NodeCond _ _ so
              | so == Just "yes" -> Sum 1
              | otherwise -> Sum 0

encodeLinkCsv :: LinkCsv -> String
encodeLinkCsv lc =
  "node_id_org,node_id_dest,distance,signal,highway,max_speed,lanes,width,bridge,tunnel,surface,service,foot,bicycle"
    <> foldr
      (\(LinkWithCond (Path g dist) (LinkCond highway max_speed lanes width bridge tunnel surface service foot bicycle) s) str ->
        let
          org :->: dest = compose g
        in 
          str
            <> "\n"
            <> show org <> ","
            <> show dest <> ","
            <> show dist <> ","
            <> show (getSum s) <> ","
            <> showMaybe highway <> ","
            <> showMaybe max_speed <> ","
            <> showMaybe lanes <> ","
            <> showMaybe width <> ","
            <> showMaybe bridge <> ","
            <> showMaybe tunnel <> ","
            <> showMaybe surface <> ","
            <> showMaybe service <> ","
            <> showMaybe foot <> ","
            <> showMaybe bicycle <> ",")
      "" lc
