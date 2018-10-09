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



type Node = Int

type Cost = Double

data OD = Node :->: Node deriving (Eq, Show)
infixr 5 :->:

instance Ord OD where
  compare od1@(n1 :->: n2) od2@(n3 :->: n4)
    | od1 == od2 = EQ
    | otherwise =
      case compare n1 n3 of
        EQ ->
          if n2 < n4 then LT else GT
        o -> o

instance Semigroup OD where
  (<>) (n1 :->: n2) (n3 :->: n4)
    | n2 == n3 = n1 :->: n4
    | otherwise = error "Semigroup OD Error."

data Link = Link Graph Cost deriving Show

instance Eq Link where
  Link g1 c1 == Link g2 c2 = g1 == g2 && c1 == c2

instance Ord Link where
  compare l1@(Link g1 c1) l2@(Link g2 c2)
    | l1 == l2 = EQ
    | c1 == c2 =
      if g1 < g2 then LT else GT
    | c1 < c2 = LT
    | otherwise = GT

instance Semigroup Link where
  Link g1 c1 <> Link g2 c2 = Link (g1 <> g2) (c1 + c2)

type Network = Map.Map OD Link

type Path = Network

data Graph = Edge OD | Graph (V.Vector OD) deriving Show

compose :: Graph -> OD
compose (Edge od) = od
compose (Graph v) = foldr1 (<>) v

composeLink :: Link -> OD
composeLink (Link g _) = compose g

instance Eq Graph where
  g1 == g2 = compose g1 == compose g2

instance Ord Graph where
  compare g1 g2
    | g1 == g2 = EQ
    | compose g1 < compose g2 = LT
    | otherwise = GT

instance Semigroup Graph where
  Edge od1 <> Edge od2 = Graph [od1, od2]
  Edge od <> Graph v = Graph $ V.cons od v
  Graph v <> Edge od = Graph $ V.snoc v od
  Graph v1 <> Graph v2 = Graph $ v1 <> v2

networkFromList :: [(OD, Cost)] -> Network
networkFromList odcs = Map.fromList $ (\(od, c) -> (od, Link (Edge od) c)) <$> odcs

insertLink :: Link -> Network -> Network
insertLink l@(Link (compose -> od) _) n =
  case n Map.!? od of
    Nothing -> Map.insert od l n
    Just _  -> n

isNextLink :: Link -> Link -> Bool
isNextLink (Link g1 _) (Link g2 _) = isNextGraph g1 g2

isNextGraph :: Graph -> Graph -> Bool
isNextGraph (Edge od1) (Edge od2)       = isNextOD od1 od2
isNextGraph (Edge od) g@(Graph v)       = isNextOD od (compose g) && notElem (invertOD od) v -- && V.notElem od v
isNextGraph g@(Graph v) (Edge od)       = isNextOD (compose g) od && notElem (invertOD od) v -- && V.notElem od v
isNextGraph g1@(Graph v1) g2@(Graph v2) = isNextOD (compose g1) (compose g2) && not (any (`elem` v1) v2) && not (any (`elem` v1) (invertOD <$> v2))

isNextOD :: OD -> OD -> Bool
isNextOD (n1 :->: n2) (n3 :->: n4) = n2 == n3 && n1 /= n4

inverseNetwork :: Network -> Network
inverseNetwork = Map.foldrWithKey (\(n1 :->: n2) (Link _ c) n -> Map.insert (n2 :->: n1) (Link (Edge (n2 :->: n1)) c) n) Map.empty

invertOD :: OD -> OD
invertOD (n1 :->: n2) = n2 :->: n1

overlap :: Graph -> Graph -> Bool
overlap (Edge od1) (Edge od2) = od1 == od2
overlap (Edge od) g@(Graph v) = od `elem` v
overlap g@(Graph v) (Edge od) = od `elem` v
overlap g1@(Graph v1) g2@(Graph v2) = any (`elem` v1) v2

overlapLink :: Link -> Link -> Bool
overlapLink (Link g1 _) (Link g2 _) = overlap g1 g2

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""



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
