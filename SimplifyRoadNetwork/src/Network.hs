{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}


module Network where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)
import           Csv.LinkCsv                (LinkCsv, LinkCond, LinkWithCond)
import           Csv.NodeCsv                (NodeCsv)
import qualified Data.Map.Lazy              as Map
import qualified Data.Vector                as V
--import           Network
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set



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



data NetworkCsv = NetworkCsv LinkCsv NodeCsv deriving (Show)
type NetworkCsvInput = NetworkCsv
type NetworkCsvOutput = NetworkCsv

simplifyNetworkCsv :: NetworkCsv -> NetworkCsv
simplifyNetworkCsv (NetworkCsv lci nci) =
  let 
    lco = simplifyLinkCsv $ longestLinkCsv lci
    nodes = f lco :: Set.Set Node
    nco = Map.filterWithKey (\node_ _ -> Set.member node_ nodes) nci
  in
    NetworkCsv lco nco

  where
    f lco = foldr (\(composeLink -> org_ :->: dest_, _) nodes_ -> (Set.insert org_ . Set.insert dest_) nodes_) Set.empty lco

simplifyLinkCsv :: LinkCsv -> LinkCsv
simplifyLinkCsv lc =
  go lc []
--V.foldr f []
  where
    go [] lco = lco
    go (uncons -> (lwc@(link, cond), lci)) lco =
      go lci $ f lco
      where
        --Nothing (交差点および行き止まり) を無視して和を計算

        f :: LinkCsv -> LinkCsv
        f lc =
          (`execState` lc) $
            do
              maybeLink1 <- g1
              maybeLink2 <- g2
              lc_ <- get
              put $ V.cons (fromJust $ maybeLink1 <> Just link <> maybeLink2, cond) lc_

        g1 :: State LinkCsv (Maybe Link)
        g1 =
          state $ \lc ->
            case V.partition (\(link_, cond_) -> isNextLink link_ link) lc of
              ([(link1, cond1)], lc1) ->
                if cond == cond1 && not (any (\(link_, _) -> isNextLink link_ link) lci)
                  then (Just link1, lc1)
                  else (Nothing, lc)
              _            -> (Nothing, lc)
        
        g2 :: State LinkCsv (Maybe Link)
        g2 =
          state $ \lc ->
            case V.partition (\(link_, cond_) -> isNextLink link link_) lc of
              ([(link2, cond2)], lc2) ->
                if cond == cond2 && not (any (\(link_, _) -> isNextLink link link_) lci)
                  then (Just link2, lc2)
                  else (Nothing, lc)
              _            -> (Nothing, lc)

uncons :: V.Vector a -> (a, V.Vector a)
uncons v = (V.head v, V.tail v)


longestLinkCsv :: LinkCsv -> LinkCsv
longestLinkCsv lc =
  go lc []
  where
    go (V.null -> True) lcs =
      foldr1 (\lc1 lc2 -> if totalDistance lc1 > totalDistance lc2 then lc1 else lc2) lcs
    go (uncons -> (lwc@(link, _), lci)) lcs =
      let
        (lcs1, lcs2) = V.partition (any (\(link_, _) -> isNextLink link_ link || isNextLink link link_)) lcs
      in
        go lci $ V.cons (V.cons lwc $ foldr (<>) [] lcs1) lcs2


totalDistance :: LinkCsv -> Double
totalDistance = foldr (\(Link _ dist, _) total -> total + dist) 0

makeNetwork :: NetworkCsv -> Network
makeNetwork (NetworkCsv lc _) = foldr (\(Link g@(compose -> od) dist, _) n -> Map.insertWith min od (Link g dist) n) Map.empty lc