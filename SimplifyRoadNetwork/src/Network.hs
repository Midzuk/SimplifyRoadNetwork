{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}


module Network where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)            
import qualified Data.Map.Strict            as Map
import qualified Data.Vector                as V
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Data.Monoid
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Function
import           Control.Monad

import           Link                       
import           Node           

import Debug.Trace

-- data NetworkCsv = NetworkCsv LinkCsv NodeCsv deriving (Show)
type ToLinks = Links
type Graph = (Nodes, Links, ToLinks)

makeGroups :: Graph -> V.Vector Graph 
makeGroups (nodes, links) = undefined
  where
    ns = Map.keysSet nodes
    undefined

    f :: Node -> Graph -> Graph
    f n (_nodes, _links) = undefined

    to :: Graph -> Graph -> Bool
    (ns1, ls1, tls1) `to` (ns2, ls2, tls2) =
      undefined

    from :: Graph -> Graph -> Bool
    g1 `from` g2 = undefined

-- uncons :: V.Vector a -> (a, V.Vector a)
-- uncons v = (V.head v, V.tail v)


{-
longestLinkCsv :: LinkCsv -> LinkCsv
longestLinkCsv lc = trace "longestLinkCsv" $ 
  go lc []
  where
    go (V.null -> True) lcs =
      foldr1 (\lc1 lc2 -> if totalDistance lc1 > totalDistance lc2 then lc1 else lc2) lcs
    go (uncons -> (lwc, lci)) lcs =
      let
        (lcs1, lcs2) = V.partition (any $ \_lwc -> link lwc `isNextLink` link _lwc || link _lwc `isNextLink` link lwc) lcs
      in
        go lci $ V.cons (V.cons lwc $ foldr (<>) [] lcs1) lcs2


totalDistance :: LinkCsv -> Double
totalDistance = trace "totalDistance" $ foldr (\_lwc total -> (+) total (distance $ link _lwc)) 0 -- 引数おかしい?
-}


--data Path = Path { graph :: Graph, cost :: Cost } deriving Show
--data LinkCond = LinkCond Highway MaxSpeed Lanes Width Bridge Tunnel Surface Service Foot Bicycle deriving (Eq, Show)
--data LinkWithCond = LinkWithCond { path :: Path, linkCond :: LinkCond, signal :: Signal } deriving (Eq, Show)
--type LinkCsv = V.Vector LinkWithCond

--data NodeCond = NodeCond { latitude :: Latitude, longitude :: Longitude, signalOut :: SignalOut } deriving (Eq, Show)
--type NodeCsv = Map.Map Node NodeCond

cutDeadEnd :: Nodes -> Links -> (Nodes, Links)
cutDeadEnd = undefined

{-
cutDeadEnd :: LinkCsv -> NodeCsv -> (LinkCsv, NodeCsv)
cutDeadEnd (longestLinkCsv -> lc_) nc_ = trace "cutDeadEnd" $ (lc', nc')
  where
    (dlc_, rlc_) = V.partition ((`deadEnd` lc_) . link) lc_
    (nlc0, rlc0) = V.partition (\_lwc -> any ((`isNextLink` link _lwc) . link) dlc_ || any ((link _lwc `isNextLink`) . link) dlc_) rlc_

    lc' = go nlc0 rlc0
    nc' =
      Set.filter
        (\_n ->
          any (\_lwc -> origin (link _lwc) == nodeId _n || destination (link _lwc) == nodeId _n) lc')
        nc_

    go nlc rlc
      | V.null dlc__ = nlc <> rlc
      | otherwise = go nlc1 rlc1
      where
        (dlc__, rlc__) = V.partition ((`deadEnd` (nlc <> rlc)) . link) nlc
        (nlc1, rlc1) =
          V.partition
            (\_lwc -> any ((`isNextLink` link _lwc) . link) dlc__ || any ((link _lwc `isNextLink`) . link) dlc__)
            (rlc <> rlc__)
-}

{-
cutDeadEnd :: LinkCsv -> NodeCsv -> (LinkCsv, NodeCsv)
cutDeadEnd lc_ nc_ = go nnc0 rnc0 lc0 
  where
    (dnc_, rnc_) = V.partition (`deadEnd` lc_) nc_
    (nnc0, rnc0) = V.partition (g dnc_ lc_) rnc_
    lc0 = f dnc_ lc_

    --特定のNodeを持たないLink
    f :: NodeCsv -> LinkCsv -> LinkCsv
    f dnc lc = V.filter (\_lwc -> origin (link _lwc) `V.notElem` (nodeId <$> dnc) && destination (link _lwc) `V.notElem` (nodeId <$> dnc)) lc

    --特定のNodeに隣接するか
    g :: NodeCsv -> LinkCsv -> Node -> Bool
    g nc lc n = nodeId n `V.elem` (foldr (\_n _ns -> _ns <> nextNode _n lc <> prevNode _n lc) [] nc)

    go :: NodeCsv -> NodeCsv -> LinkCsv -> (LinkCsv, NodeCsv)
    go nnc rnc lc
      | V.length dnc__ == 0 = (lc, nnc <> rnc)
      | otherwise = trace "aaa" $ go nnc1 rnc1 lc1
      where

        (dnc__, rnc__) = V.partition (`deadEnd` lc) nnc
        (nnc1, rnc1) = V.partition (g dnc__ lc) (rnc <> rnc__)
        lc1 = f dnc__ lc
-}



        
{-
deadEnd :: Node -> LinkCsv -> Bool
deadEnd n lc = nearbyNodeNum n lc < 2
-}

deadEnd :: Link -> LinkCsv -> Bool
deadEnd l lc = trace "deadEnd" $ not (any (\_lwc -> l `isNextLink` link _lwc) lc && any (\_lwc -> link _lwc `isNextLink` l) lc)


--実験

orgLinkCsv :: Node -> LinkCsv -> LinkCsv
orgLinkCsv n = trace "orgLinkCsv" $ V.filter (\_lwc -> origin (link _lwc) == nodeId n)

destLinkCsv :: Node -> LinkCsv -> LinkCsv
destLinkCsv n = trace "destLinkCsv" $ V.filter (\_lwc -> destination (link _lwc) == nodeId n)

nextNode :: Node -> LinkCsv -> Set.Set NodeId
nextNode n lc = trace "nextNode" $ V.foldr Set.insert Set.empty $ destination . link <$> orgLinkCsv n lc

--isNextNode :: NodeId -> NodeId -> LinkCsv -> Bool
--isNextNode ni2 ni1 lc = ni2 `V.elem` (nextNode ni1 lc)

prevNode :: Node -> LinkCsv -> Set.Set NodeId
prevNode n lc = trace "prevNode" $ V.foldr Set.insert Set.empty $ origin . link <$> destLinkCsv n lc

--isPrevNode :: NodeId -> NodeId -> LinkCsv -> Bool
--isPrevNode ni1 ni2 lc = ni1 `V.elem` (prevNode ni2 lc)

nearbyNodeNum :: Node -> LinkCsv -> Int
nearbyNodeNum n lc = trace "nearbyNodeNum" $ Set.size $ nextNode n lc <> prevNode n lc


{-
nextLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
nextLinkCsv l lc = V.partition (\_lwc -> destination (link _lwc) /= origin l) $ orgLinkCsv (destination l) lc

prevLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
prevLinkCsv l lc = V.partition (\_lwc -> origin (link _lwc) /= destination l) $ destLinkCsv (origin l) lc
-}

intersection :: LinkCsv -> NodeCsv -> (NodeCsv, NodeCsv)
intersection lc = trace "intersection" $ Set.partition (\_n -> nearbyNodeNum _n lc > 2)

--異なる性質の道路の継ぎ目 (要cutDeadEnd処理済み)
joint :: LinkCsv -> NodeCsv -> NodeCsv
joint lc =
  trace "joint" $ Set.filter
    (\_n -> Set.size ((Set.union `on` (V.foldr Set.insert Set.empty . (linkCond <$>))) (orgLinkCsv _n lc) (destLinkCsv _n lc)) == 2)

{-
nearbyLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
nearbyLinkCsv l =
  V.partition
    (\_lwc ->
      l `isNextLink` (link _lwc) || (link _lwc) `isNextLink` l　|| (invertLink l) `isNextLink` (link _lwc) || (link _lwc) `isNextLink` (invertLink l))



  where
    f n
      | V.length plc == 1 && V.length nlc == 1 = (linkCond . V.head $ prevLinkCsv n lc) /= (linkCond . V.head $ nextLinkCsv n lc)
      | otherwise = False
-}

simplifyNetworkCsv :: NetworkCsv -> NetworkCsv
simplifyNetworkCsv (NetworkCsv lc_ nc_) =
  trace "simplifyNetworkCsv" $ NetworkCsv (g <$> V.filter (\_lwc -> origin (link _lwc) `Set.member` Set.map nodeId nc0) lc0) nc0 --要修正
  where
    (lc0, nc__) = trace "cutDeadEnd" $ cutDeadEnd lc_ nc_
    (inc, rnc) = trace "intersection" $ intersection lc0 nc__
    jnc = trace "joint" $ joint lc0 rnc
    nc0 = inc <> jnc

    f :: Link -> Link
    f l@((:->:) org dest _)
      | dest `Set.member` Set.map nodeId nc0 = l
      | otherwise = l <> f (link . V.head $ V.filter ((`isNextLink` l) . link) lc0)
    
    g :: LinkWithCond -> LinkWithCond
    g lwc@(LinkWithCond l cond s) = LinkWithCond (f l) cond s
