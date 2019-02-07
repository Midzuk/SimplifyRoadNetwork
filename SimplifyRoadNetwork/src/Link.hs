{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}

module Link where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, (.:))
import qualified Data.Map.Strict      as Map

import           Data.Maybe           (isJust)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified System.Directory     as Dir
import           Data.Monoid
import qualified Data.Set             as Set
import           Debug.Trace

import           Node



--type Cost = Double
type Distance = Double

data Link = 
  (:->:)
    { origin :: Node
    , destination :: Node 
    }
  deriving (Eq, Ord, Show)
infixr 5 :->:

instance Semigroup Link where
  (org1 :->: dest1) <> (org2 :->: dest2)
    | dest1 == org2 = org1 :->: dest2
    | otherwise = error "Semigroup Link Error."

-- isNextLink :: Link -> Link -> Bool
-- (org2 :->: dest2) `isNextLink` (org1 :->: dest1) = dest1 == org2 && org1 /= dest2

data LinkCond =
  LinkCond
    { distance :: Distance
    , linkcond :: (Highway, MaxSpeed, Lanes, Width, Bridge, Tunnel, Surface, Service, Foot, Bicycle)
    }
  deriving (Eq, Ord, Show)

instance Semigroup LinkCond where
  LinkCond d1 lc1 <> LinkCond d2 lc2
    | lc1 == lc2 = LinkCond (d1 + d2) lc1
    | otherwise = error "Semigroup LinkCond Error."

type Links =
  ( Map.Map Node (V.Vector Node) -- 隣り合うNode
  , Map.Map Link LinkCond
  )

type Origin = Node
type Destination = Node

type Oneway = Maybe T.Text

type Highway = Maybe T.Text
type MaxSpeed = Maybe T.Text
type Lanes = Maybe T.Text
type Width = Maybe T.Text
type Bridge = Maybe T.Text
type Tunnel = Maybe T.Text
type Surface = Maybe T.Text
type Service = Maybe T.Text
type Foot = Maybe T.Text
type Bicycle = Maybe T.Text

-- type Signal = Sum Int

data LinkCsv =
  LinkCsv
    Origin
    Destination
    Distance
    Oneway
    Highway
    MaxSpeed
    Lanes
    Width
    Bridge
    Tunnel
    Surface
    Service
    Foot
    Bicycle
  deriving (Show)

-- type Graph = V.Vector Node


{-
showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""
-}


instance FromNamedRecord LinkCsv where
  parseNamedRecord m =
    LinkCsv
      <$> m .: "node_id_org"
      <*> m .: "node_id_dest"
      <*> m .: "distance"
      <*> m .: "oneway"
      <*> m .: "highway"
      <*> m .: "max_speed"
      <*> m .: "lanes"
      <*> m .: "width"
      <*> m .: "bridge"
      <*> m .: "tunnel"
      <*> m .: "surface"
      <*> m .: "service"
      <*> m .: "foot"
      <*> m .: "bicycle"


decodeLinkCsv :: FilePath -> IO Links
decodeLinkCsv fp = trace "decodeLinkCsv" $ do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector LinkCsv)
  return $ makeLinks ls

makeLinks :: V.Vector LinkCsv -> Links
makeLinks lco = foldr f (Map.empty, Map.empty) lco
  where
    f (LinkCsv org dest dist oneway highway maxSpeed lanes width bridge tunnel surface service foot bicycle) (mn, ml)
      | oneway == Just "yes" =
        ( ( Map.insertWith (<>) org (V.singleton dest)
          . Map.insertWith (<>) dest (V.singleton org)
          )
            mn
        , Map.insert 
            (org :->: dest)
            (LinkCond dist (highway, maxSpeed, lanes, width, bridge, tunnel, surface, service, foot, bicycle))
            ml
        )

      | oneway == Just "-1" =
        ( ( Map.insertWith (<>) org (V.singleton dest)
          . Map.insertWith (<>) dest (V.singleton org)
          )
            mn
        ,  Map.insert
            (dest :->: org)
            (LinkCond dist (highway, maxSpeed, lanes, width, bridge, tunnel, surface, service, foot, bicycle))
            ml
        )

      | otherwise =
        ( ( Map.insertWith (<>) org (V.singleton dest)
          . Map.insertWith (<>) dest (V.singleton org)
          )
            mn
        , ( Map.insert
              (org :->: dest)
              (LinkCond dist (highway, maxSpeed, lanes, width, bridge, tunnel, surface, service, foot, bicycle))
            .
            Map.insert
              (dest :->: org)
              (LinkCond dist (highway, maxSpeed, lanes, width, bridge, tunnel, surface, service, foot, bicycle))
          )
          ml
        )

{-
encodeLinks :: Links -> String
encodeLinks lc =
  "node_id_org,node_id_dest,distance,signal,highway,max_speed,lanes,width,bridge,tunnel,surface,service,foot,bicycle"
    <> foldr
      (\(LinkWithCond ((:->:) org dest dist) (LinkCond highway max_speed lanes width bridge tunnel surface service foot bicycle) s) str ->
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
-}