module Lawvere.Sketch where

import Lawvere.Core

data Sketch = Sketch
  { name :: UcIdent,
    points :: [LcIdent],
    arrows :: [(LcIdent, LcIdent)]
  }
