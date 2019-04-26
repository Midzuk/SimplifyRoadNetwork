{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}


module Network where

import           Control.Monad.State.Strict (State, evalState, execState,
                                             runState, state, put, get)            
import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S
import           Data.Monoid
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Function
import           Control.Monad

import           Link                       
import           Node           

import　　　　　　 Debug.Trace

type Graph = (Node, NodeCond, S.Set Node, M.Map Node LinkCond)
type Graphs = [Graph]

makeGraph :: Nodes -> Links -> Graphs
makeGraph ns (mnns, mllc) =
  (\(n, nc) -> (n, nc, mnns M.! n, M.mapKeys (\l -> destination l) . M.filterWithKey (\l _ -> origin l == n) $ mllc)) <$> M.toList ns

simplify :: Graphs -> Graphs
simplify (g':gs') = go g' $ simplify gs'
  where
    go g1 (g2:gs2) =
      undefined
    go g1 [] =
      [g1]
