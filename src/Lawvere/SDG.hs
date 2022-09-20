module Lawvere.SDG where

import qualified Data.Map.Strict as Map
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Scalar
import Protolude

data Val
  = Rec (Map Label Val)
  | Tag Label Val
  | Sca Sca

instance Disp Val where
  disp (Rec _) = "rec TODO"
  disp (Tag _ _) = "tag TODO"
  disp (Sca s) = disp s

type Tops = Map LcIdent Expr

data Ar
  = PointAr (Val -> Val)
  | Simul Val (Val -> Val)

eval :: Tops -> Expr -> Ar
eval tops e = case e of
  EId -> PointAr identity
  Top name -> case Map.lookup name tops of
    Just e' -> eval tops e'
    Nothing -> panic "SDG undefined top"
  EIntegrate init _deriv -> Simul initSimul nextSimul
    where
      initF = eval tops init
      -- derivF = eval tops deriv
      initSimul = case initF of
        PointAr f -> f (Rec mempty)
        Simul _i _f -> panic "TODO"
      nextSimul _current = panic "TODO"
  _ -> panic ("SDG: unhandled case: " <> render e)

evalProg :: Decls -> [Val]
evalProg _decls = panic "TODO"
  -- where
  --   ar = eval (namedToplevels decls) (Top (LcIdent "main"))

namedToplevels :: Decls -> Map LcIdent Expr
namedToplevels decls =
  Map.fromList (mapMaybe named decls)
  where
    named = \case
      DAr _ name _ e -> Just (name, e)
      _ -> Nothing

