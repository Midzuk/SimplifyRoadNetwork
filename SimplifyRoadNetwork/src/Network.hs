{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}


module Network where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)            
import qualified Data.Map.Lazy              as Map
import qualified Data.Vector                as V
--import           Network
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Data.Monoid
import           Data.Either.Combinators    (rightToMaybe)

import           Link                       
import           Node           



data NetworkCsv = NetworkCsv LinkCsv NodeCsv deriving (Show)

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
{-
cutDeadEnd :: LinkCsv -> LinkCsv
cutDeadEnd lc = V.filter (\(LinkWithCond p _ _) -> any (\(LinkWithCond _p _ _) -> p `isNextPath` _p) lc && any (\_p -> _p `isNextPath` p) lc) lc
-}

cutDeadEnd :: LinkCsv -> LinkCsv
cutDeadEnd lc =
  (`execState` lc) $ go (path <$> lc)
  where
    go :: V.Vector Path -> State LinkCsv ()
    go ps =
      do
        ps1 <- f ps
        lc1 <- get
        if V.null ps1
          then put lc1
          else go ps1

    f :: V.Vector Path -> State LinkCsv (V.Vector Path)
    f ps = state $ \ lc ->
      let
        dps = V.filter (`deadEnd` lc) ps
        lc1 = V.filter ((`V.notElem` dps) . path) lc
      in
        (dps, lc1)

deadEnd :: Path -> LinkCsv -> Bool
deadEnd p lc = not (any (\_lwc -> p `isNextPath` path _lwc) lc && any (\_lwc -> path _lwc `isNextPath` p) lc)

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
        (lcs1, lcs2) = V.partition (any $ \_lwc -> path lwc `isNextPath` path _lwc || path _lwc `isNextPath` path lwc) lcs
      in
        go lci $ V.cons (V.cons lwc $ foldr (<>) [] lcs1) lcs2


totalDistance :: LinkCsv -> Double
totalDistance = foldr (\(LinkWithCond (Path _ dist) _ _) total -> total + dist) 0

{-
makeNetwork :: NetworkCsv -> Network
makeNetwork (NetworkCsv lc _) = foldr (\(Path g@(compose -> od) dist, _) n -> Map.insertWith min od (Path g dist) n) Map.empty lc
-}