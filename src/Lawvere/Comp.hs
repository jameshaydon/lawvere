module Lawvere.Comp where

import Control.Lens
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import Lawvere.Core
import Lawvere.Expr
import Lawvere.Scalar
import Protolude

-- See:
-- http://okmij.org/ftp/Computation/having-effect.html#stable

type Hask = Dom -> Comp

data Dom
  = Sca Sca
  | Rec (Map Label Dom)
  | Tag Label Dom
  | Fun Hask

data Req
  = GetTop LcIdent
  | IErr

data Comp
  = Done Dom
  | Req Req Dom Hask

instance {-# OVERLAPPING #-} Semigroup Hask where
  f <> g = \x -> bind (f x) g

instance {-# OVERLAPPING #-} Monoid Hask where
  mempty = Done

-- bind e k expresses the common pattern of consuming the value of e. If that
-- expression is not a value but sends a request, the request is propagated,
-- with the updated return address.
bind :: Comp -> (Dom -> Comp) -> Comp
bind (Done x) k = k x
bind (Req r x k1) k2 = Req r x (\y -> bind (k1 y) k2)

-- Sequence a bunch of independent computations.
sequ :: [Comp] -> ([Dom] -> Comp) -> Comp
sequ [] k = k []
sequ (c : cs) k = bind c $ \x -> sequ cs (k . (x :))

compile :: Expr -> Hask
compile = \case
  Lit s -> const (Done (Sca s))
  Top name -> \x -> Req (GetTop name) x Done
  Inj lab -> Done . Tag lab
  Proj lab -> \case
    Rec (Map.lookup lab -> Just x) -> Done x
    _ -> ierr "bad proj"
  Comp fs -> foldMap compile fs
  Cone fs -> \x ->
    sequ
      (($ x) . compile . snd <$> fs)
      (Done . Rec . Map.fromList . zip (componentLabel . fst <$> fs))
  Tuple fs -> compile (tupleToCone fs)
  CoCone fs -> \case
    Tag (flip lookup fs' -> Just f) x -> f x
    _ -> ierr "bad cocone"
    where
      fs' = fs & traverse . _2 %~ compile
  Distr l -> \case
    Rec r@(Map.lookup l -> Just (Tag t x)) -> Done (Tag t (Rec (Map.insert l x r)))
    _ -> ierr "bad distr"
  ELim _limOfFunctors -> functor
    where
      functor :: Dom -> Comp
      functor _f = Done (Fun undefined) -- (Fun g)
        -- where
        --   g :: Hask
        --   g (Rec r) = let foo = Map.toList (Map.mapWithKey go r)
        --                in undefined
        --     where
        --       go :: Label -> Dom -> Comp
        --       go = undefined
  _ -> undefined

{-
ELim limOfFunctors -> functor
    where
      functor :: Val -> IO Val
      functor f = pure (VFun g)
        where
          g :: Val -> IO Val
          g (Rec r) = Rec <$> Map.traverseWithKey go r
            where
              go :: Label -> Val -> IO Val
              go label x = case lookup label limOfFunctors of
                Just func -> do
                  res <- evalAr tops func f
                  case res of
                    VFun resF -> resF x
                    _ -> panic "bad ELim"
                Nothing -> panic "bad ELim"
          g _ = panic "bad ELim"
-}

ierr :: Text -> Comp
ierr m = Req IErr (Sca (Str m)) Done
