{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns #-}


module Network where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)
import           Link                       
import           Node                       (NodeCsv)
import qualified Data.Map.Lazy              as Map
import qualified Data.Vector                as V
--import           Network
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set



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