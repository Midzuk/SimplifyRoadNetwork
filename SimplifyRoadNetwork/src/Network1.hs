{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}


module Network1 where

{-
import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)            
import qualified Data.Map.Lazy              as Map
import qualified Data.Vector                as V
--import           Network
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Data.Monoid
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Function
import           Control.Monad

import           Link                       
import           Node           

import Debug.Trace


data NetworkCsv = NetworkCsv LinkCsv NodeCsv deriving (Show)

{-
simplifyNetworkCsv :: NetworkCsv -> NetworkCsv
simplifyNetworkCsv (NetworkCsv lci nci) =
  let 
    lco = cutDeadEnd . simplifyLinkCsv $ longestLinkCsv lci
    nodes = f lco :: Set.Set Node
    nco = Map.filterWithKey (\_node _ -> Set.member _node nodes) nci
  in
    NetworkCsv lco nco

  where
    f lco = foldr (\(LinkWithCond (composePath -> _org :->: _dest) _ _) _nodes -> (Set.insert _org . Set.insert _dest) _nodes) Set.empty lco


simplifyLinkCsv :: LinkCsv -> LinkCsv
simplifyLinkCsv lc =
  go lc []
  where
    go [] lco = lco
    go (uncons -> (lwc@(LinkWithCond p cond s), lci)) lco =
      go lci $ f lco
      where
        --Nothing (交差点および行き止まり) を無視して和を計算

        f :: LinkCsv -> LinkCsv
        f lc =
          (`execState` lc) $
            do
              maybeLwc1 <- g True
              maybeLwc2 <- g False

              let maybeS1 = signal <$> maybeLwc1
              let maybeS2 = signal <$> maybeLwc2

              let maybeP1 = path <$> maybeLwc1
              let maybeP2 = path <$> maybeLwc2

              _lc <- get
              put $ V.cons (LinkWithCond (fromJust $ maybeP1 <> Just p <> maybeP2) cond (fromJust $ maybeS1 <> Just s <> maybeS2)) _lc

        g :: Bool -> State LinkCsv (Maybe LinkWithCond)
        g b =
          state $ \lc ->
            case V.partition (\_lwc -> h (path _lwc)) lc of
              ([lwc1], lc1) ->
                if cond == linkCond lwc1 && not (any (\_lwc -> h (path _lwc)) lci)
                  then (Just lwc1, lc1)
                  else (Nothing, lc)
              _            -> (Nothing, lc)
          where
            h = if b then (p `isNextPath`) else (`isNextPath` p)
        
        {-
        g2 :: State LinkCsv (Maybe LinkWithCond)
        g2 =
          state $ \lc ->
            case V.partition (\_lwc -> path _lwc `isNextPath` p) lc of
              ([lwc2], lc2) ->
                if cond == linkCond lwc2 && not (any (\_lwc -> path _lwc `isNextPath` p) lci)
                  then (Just lwc2, lc2)
                  else (Nothing, lc)
              _            -> (Nothing, lc)
        -}
-}
{-
cutDeadEnd :: LinkCsv -> LinkCsv
cutDeadEnd lc = V.filter (\(LinkWithCond p _ _) -> any (\(LinkWithCond _p _ _) -> p `isNextPath` _p) lc && any (\_p -> _p `isNextPath` p) lc) lc
-}

{-
deadEnd :: Path -> LinkCsv -> Bool
deadEnd p lc
  | nps == [] || pps == [] = False
  | otherwise = undefined
  where
    nps = nextPath p lc
    pps = prevPath p lc

nextPath :: Path -> LinkCsv -> V.Vector Path
nextpath p lc = path <$> V.filter (\_lwc -> path _lwc `isNextPath` p) lc

prevPath :: Path -> LinkCsv -> V.Vector Path
prevPath p lc = path <$> V.filter (\_lwc -> p `isNextPath` path _lwc) lc
-}

uncons :: V.Vector a -> (a, V.Vector a)
uncons v = (V.head v, V.tail v)


longestLinkCsv :: LinkCsv -> LinkCsv
longestLinkCsv lc =
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
totalDistance = foldr (\_lwc total -> (+) total (distance $ link _lwc)) 0

{-
makeNetwork :: NetworkCsv -> Network
makeNetwork (NetworkCsv lc _) = foldr (\(Path g@(compose -> od) dist, _) n -> Map.insertWith min od (Path g dist) n) Map.empty lc
-}


--data Path = Path { graph :: Graph, cost :: Cost } deriving Show
--data LinkCond = LinkCond Highway MaxSpeed Lanes Width Bridge Tunnel Surface Service Foot Bicycle deriving (Eq, Show)
--data LinkWithCond = LinkWithCond { path :: Path, linkCond :: LinkCond, signal :: Signal } deriving (Eq, Show)
--type LinkCsv = V.Vector LinkWithCond

--data NodeCond = NodeCond { latitude :: Latitude, longitude :: Longitude, signalOut :: SignalOut } deriving (Eq, Show)
--type NodeCsv = Map.Map Node NodeCond

{-
cutDeadEnd :: LinkCsv -> LinkCsv
cutDeadEnd lc = go dlc_ rlc_
  where
    (dlc_, rlc_) = V.partition (\_lwc -> ((`deadEnd` lc) . origin $ link _lwc) || ((`deadEnd` lc) . destination $ link _lwc)) lc
    go dlc rlc
      | V.null dlc = trace "ddd" $ rlc
      | otherwise = trace "aaa" $ go dlc2 (rlc1 <> rlc2)
      where
        (dlc1, rlc1) = trace "bbb" $ foldr (\_lwc _lc -> _lc <> nearbyLinkCsv (link _lwc) rlc) ([], []) dlc
        (dlc2, rlc2) = trace "ccc" $ V.partition (\_lwc -> ((`deadEnd` lc) . origin $ link _lwc) || ((`deadEnd` lc) . destination $ link _lwc)) dlc1
-}

cutDeadEnd :: LinkCsv -> NodeCsv -> NodeCsv
cutDeadEnd lc_ nc_ = undefined
  where
    (dnc, rnc1) = V.partition ((`deadEnd` lc_) . nodeId) nc_
    (nnc, rnc2) = undefined

    go :: LinkCsv -> NodeCsv -> NodeCsv -> NodeCsv
    go lc nc = undefined

deadEnd :: NodeId -> LinkCsv -> Bool
deadEnd ni lc = nearbyNodeNum ni lc < 2

{-
deadEnd :: Link -> LinkCsv -> Bool
deadEnd l lc = not (any (\_lwc -> l `isNextLink` link _lwc) lc && any (\_lwc -> link _lwc `isNextLink` l) lc)
-}

--実験

orgLinkCsv :: NodeId -> LinkCsv -> LinkCsv
orgLinkCsv ni = V.filter (\_lwc -> (origin $ link _lwc) == ni)

destLinkCsv :: NodeId -> LinkCsv -> LinkCsv
destLinkCsv ni = V.filter (\_lwc -> (destination $ link _lwc) == ni)

nextNode :: NodeId -> LinkCsv -> V.Vector NodeId
nextNode ni lc = (destination . link) <$> orgLinkCsv ni lc

isNextNode :: NodeId -> NodeId -> LinkCsv -> Bool
isNextNode ni2 ni1 lc = ni2 `V.elem` (nextNode ni1 lc)

prevNode :: NodeId -> LinkCsv -> V.Vector NodeId
prevNode ni lc = (origin . link) <$> destLinkCsv ni lc

isPrevNode :: NodeId -> NodeId -> LinkCsv -> Bool
isPrevNode ni1 ni2 lc = ni1 `V.elem` (prevNode ni2 lc)

nearbyNodeNum :: NodeId -> LinkCsv -> Int
nearbyNodeNum ni lc = Set.size $ (Set.union `on` V.foldr Set.insert Set.empty) (nextNode ni lc) (prevNode ni lc)


{-
nextLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
nextLinkCsv l lc = V.partition (\_lwc -> destination (link _lwc) /= origin l) $ orgLinkCsv (destination l) lc

prevLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
prevLinkCsv l lc = V.partition (\_lwc -> origin (link _lwc) /= destination l) $ destLinkCsv (origin l) lc
-}

nearbyLinkCsv :: Link -> LinkCsv -> (LinkCsv, LinkCsv)
nearbyLinkCsv l =
  V.partition
    (\_lwc ->
      l `isNextLink` (link _lwc) || (link _lwc) `isNextLink` l　|| (invertLink l) `isNextLink` (link _lwc) || (link _lwc) `isNextLink` (invertLink l))

intersection :: LinkCsv -> NodeCsv -> (NodeCsv, NodeCsv)
intersection lc = V.partition (\_n -> nearbyNodeNum (nodeId _n) lc > 2)

--異なる性質の道路の継ぎ目 (要cutDeadEnd処理済み)
joint :: LinkCsv -> NodeCsv -> NodeCsv
joint lc nc = V.filter (\_n -> Set.size ((Set.union `on` (V.foldr Set.insert Set.empty . (linkCond <$>))) (orgLinkCsv _n lc) (destLinkCsv _n lc)) > 1) nc


{-
  where
    f n
      | V.length plc == 1 && V.length nlc == 1 = (linkCond . V.head $ prevLinkCsv n lc) /= (linkCond . V.head $ nextLinkCsv n lc)
      | otherwise = False
-}

simplifyNetworkCsv :: NetworkCsv -> NetworkCsv
simplifyNetworkCsv (NetworkCsv lc_ nc_) =
  NetworkCsv (longestLinkCsv $ g <$> (V.filter (\_lwc -> origin (link _lwc) `V.elem` (nodeId <$> nc)) lc)) nc
  where
    lc = cutDeadEnd lc_
    (i, rnc) = intersection lc nc_
    j = joint lc rnc
    nc = i <> j

    f :: Link -> Link
    f l@((:->:) org dest _)
      | dest `Map.member` nc = trace "g" $ l
      | otherwise = l <> f (link . V.head $ trace "f" $ V.filter ((`isNextLink` l) . link) lc)
    
    g :: LinkWithCond -> LinkWithCond
    g lwc@(LinkWithCond l cond s) = LinkWithCond (f l) cond s
-}