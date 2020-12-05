module Lawvere.Prim where

import Lawvere.Disp
import Protolude

data Prim
  = PPlus
  | PIncr
  | PApp
  deriving stock (Show)

instance Disp Prim where
  disp = \case
    PPlus -> "prim[plus]"
    PIncr -> "prim[incr]"
    PApp -> "prim[app]"
