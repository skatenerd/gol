{-# LANGUAGE DeriveGeneric #-}
module Types where

import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import GHC.Generics

data Point = Point {pX :: Integer, pY :: Integer} deriving (Eq, Ord, Show, Generic)
instance Aeson.ToJSON Point
instance Aeson.FromJSON Point

type GameState = S.Set Point
