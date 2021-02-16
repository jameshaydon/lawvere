module Lawvere.Eval where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.List (foldr1, lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Ob
import Lawvere.Scalar
import Paths_lawvere
import Prettyprinter
import Protolude

data FreydDict = FreydDict
  { inj :: Fun,
    handlers :: Map LcIdent Fun,
    sumDistr :: Fun
  }

data Val
  = Rec (Map Label Val)
  | Tag Label Val
  | Sca Sca
  | VFun Fun

type Fun = Val -> IO Val

instance Disp Val where
  disp = \case
    Sca s -> disp s
    Rec r -> commaBrace '=' (Map.toList r)
    Tag t v -> parens (disp t <> "." <+> disp v)
    VFun _ -> "<unshowable>"

data Interp = Interp
  { iInj :: Fun,
    iSum :: Fun,
    iSide :: Expr,
    iHandlers :: Map LcIdent Fun
  }

data Top
  = TFun Fun
  | TExpr Expr
  | TInterp Interp
  | TFreyd Expr
  | TEffInterp EffInterp
  | TEffCat EffCat
  | TCat Category

type Tops = Map Ident Top

evalAr :: Tops -> Expr -> Fun
evalAr tops = \case
  EPrim prim -> evalPrim prim
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
                    _ -> panic "bad ELim 1"
                Nothing -> panic $ "couldn't find " <> render label <> " - " <> render x <> " - " <> render (Rec r)
          g _ = panic "bad ELim 3"
  ECoLim colimOfFunctors -> functor
    where
      functor :: Val -> IO Val
      functor f = pure (VFun g)
        where
          g :: Val -> IO Val
          g (Tag tag x) = case lookup tag colimOfFunctors of
            Just func -> do
              res <- evalAr tops func f
              case res of
                VFun resF -> Tag tag <$> resF x
                _ -> panic "bad EColim"
            Nothing -> panic "bad EColim"
          g _ = panic "bad EColim"
  EConst e -> const (pure (VFun (evalAr tops e)))
  Top i -> \v -> case Map.lookup (Lc i) tops of
    Just (TFun f) -> f v
    Just (TFreyd e) -> evalAr tops e v
    _ -> panic $ "undefined: " <> render i
  InterpolatedString ps -> \v -> do
    let go (ISRaw t) = pure t
        go (ISExpr e) = do
          s_ <- evalAr tops e v
          case s_ of
            Sca (Str s) -> pure s
            _ -> panic "bad string interpolation"
    ss <- traverse go ps
    pure (Sca (Str (mconcat ss)))
  Lit x -> const (pure (Sca x))
  Inj i -> pure . Tag i
  Distr l -> \case
    Rec r -> case Map.lookup l r of
      Just y -> case y of
        Tag t z -> pure (Tag t (Rec (Map.insert l z r)))
        _ -> panic $ "bad distr: " <> render l <> " - " <> render y
      Nothing -> panic "bad2"
    v -> panic $ "bad distr 3: " <> render v
  Proj l -> \case
    v@(Rec xs) -> case Map.lookup l xs of
      Just y -> pure y
      Nothing -> panic ("bad record projection, no key: " <> render l <> " in " <> render v)
    v -> panic ("bad record projection, not record: " <> render l <> "-\n-" <> render v)
  Comp fs -> foldr' comp pure fs
    where
      comp e cur = evalAr tops e >=> cur
  Tuple fs -> evalAr tops (tupleToCone fs)
  Cone fs -> mkCone fs
  CoCone fs -> evalAr tops (Top (LcIdent "sumPreserver")) >=> mkCoCone fs
  EFunApp "io" e ->
    evalAr
      ( Map.fromList
          [ (Lc "putLine", TFun lawPutLine),
            (Lc "getLine", TFun (const (Sca . Str <$> getLine))),
            ( Lc "side",
              TExpr
                ( Cone
                    [ (ConeComponent Pure (LNam (LcIdent "pur")), Proj (LNam (LcIdent "pur"))),
                      (ConeComponent Pure (LNam (LcIdent "eff")), Comp [Proj (LNam (LcIdent "eff")), Top "eff"])
                    ]
                )
            )
          ]
          <> tops
      )
      e
  EFunApp name e ->
    case Map.lookup (Lc name) tops of
      Just (TFun ff) -> \x -> do
        let f = VFun (evalAr tops e)
        g <- ff f
        case g of
          VFun g' -> g' x
          v -> panic $ "bad efunapp: " <> render v
      Just (TInterp Interp {..}) ->
        evalAr (Map.mapKeys Lc (TFun <$> iHandlers) <> Map.fromList [(Lc "i", TFun iInj), (Lc "sumPreserver", TFun iSum), (Lc "side", TExpr iSide)] <> tops) e
      Just (TEffCat effcat) -> case Map.lookup (Uc (effcat ^. #effCatName)) tops of
        Just (TCat c) ->
          let e' = evalEffCat tops c effcat e
           in evalAr tops e'
        _ -> panic $ "bad efunapp"
      _ -> panic $ "bad efunapp: " <> render name
  -- Curry _ _ -> panic "curry"
  Object _ -> const (pure (VFun pure))
  CanonicalInj e -> evalAr tops (EFunApp (LcIdent "i") e)
  Side lab e -> tr ("before sidecar: " <> render lab) >=> applyInj (sidePrep (LNam lab)) >=> tr "after prep" >=> sidecar e >=> tr "after side" >=> applyInj (sideUnprep (LNam lab))
  BinOp o f g -> \v -> do
    x <- evalAr tops f v
    y <- evalAr tops g v
    case (x, y) of
      (Sca a, Sca b) -> pure (binOp Sca toValBool o a b)
      _ -> panic "bad binop"
  SumInjLabelVar _ -> panic "label var remains"
  SumUniCoconeVar _ -> panic "cocone var remains"
  SidePrep lab -> sidePrep lab
  SideUnprep lab -> sideUnprep lab
  where
    lawPutLine = \case
      Sca (Str s) -> do
        putStrLn s
        pure (Rec mempty)
      _ -> panic "bad putLine"
    tr :: Text -> Fun
    tr _t v = do
      --putStrLn $ "=> TRACE: " <> t
      --putStrLn (render v)
      --putStrLn ("--------" :: Text)
      pure v
    sidecar e = case getTop "side" of
      TExpr sideE -> evalAr (Map.insert (Lc "eff") (TFun (evalAr tops e)) tops) sideE
      TFun f -> \v -> do
        g <- f (VFun (evalAr tops e))
        case g of
          VFun g' -> g' v
          _ -> panic "bad sidecar"
      _ -> panic "bad sidecar"
    getTop name = case Map.lookup (Lc name) tops of
      Just top -> top
      _ -> panic $ "could not get top: " <> render name
    getTopFun name = case getTop name of
      TFun f -> f
      _ -> panic "bad getTopFun"
    sidePrep :: Label -> Fun
    sidePrep lab = \case
      Rec r@(Map.lookup lab -> Just x) ->
        pure (Rec (Map.fromList [(LNam "eff", x), (LNam "pur", Rec (Map.delete lab r))]))
      v ->
        panic $ "bad side prep: " <> render lab <> " - " <> render v
    sideUnprep :: Label -> Fun
    sideUnprep lab = \case
      Rec r | Just x <- Map.lookup (LNam "eff") r, Just (Rec rest) <- Map.lookup (LNam "pur") r -> pure (Rec (Map.insert lab x rest))
      v -> panic $ "bad side unprep: " <> render v
    applyInj :: Fun -> Fun
    applyInj f = \x -> do
      f_ <- getTopFun "i" (VFun f)
      case f_ of
        VFun g -> g x
        _ -> panic "bad apply inj"
    mkCone fs =
      let ars = second (evalAr tops) <$> fs
       in \x -> do
            ys <- traverse (\(l, f) -> (componentLabel l,) <$> f x) ars
            pure (Rec (Map.fromList ys))

    mkCoCone fs =
      let ars = Map.fromList (second (evalAr tops) <$> fs)
       in \case
            Tag l x -> case Map.lookup l ars of
              Just f -> f x
              Nothing -> panic ("bad cocone 1: " <> render l <> " " <> render x)
            v -> panic ("bad cocone 2:\n\n" <> render (CoCone fs) <> "\n - \n" <> render v)

evalEffCat :: Tops -> Category -> EffCat -> Expr -> Expr
evalEffCat tops cate@Category {..} effcat@EffCat {..} = \case
  CanonicalInj e -> canInj e
  Top na -> case Map.lookup (Lc na) tops of
    Just top -> case top of
      TFreyd e -> evalEffCat tops cate effcat e
      _ -> panic "effcat: bad top"
    Nothing -> case Map.lookup (Uc (UcIdent (effCatName ^. #getUcIdent <> "." <> na ^. #getLcIdent))) tops of
      Just (TExpr e) -> e
      _ -> panic $ "effcat: badtop: " <> render na
  Comp [] -> id
  Comp [e] -> evalEffCat tops cate effcat e
  Comp es -> foldr1 (curry comp) (evalEffCat tops cate effcat <$> es)
  Side lab e ->
    foldr1
      (curry comp)
      [ canInj (SidePrep (LNam lab)),
        side (evalEffCat tops cate effcat e),
        canInj (SideUnprep (LNam lab))
      ]
  CoCone es -> case sumUni of
    Just su -> su (es & each . _2 %~ evalEffCat tops cate effcat)
    Nothing -> panic "effcat: no sum"
  e -> panic $ "effcat: " <> render e

lkp :: Label -> Map Label a -> Maybe a
lkp = Map.lookup

toValBool :: Bool -> Val
toValBool True = Tag (LNam "true") (Rec mempty)
toValBool False = Tag (LNam "false") (Rec mempty)

evalPrim :: Prim -> Fun
evalPrim = \case
  PrimOp o ->
    \case
      Rec r
        | Just (Sca a) <- lkp (LPos 1) r,
          Just (Sca b) <- lkp (LPos 2) r ->
          pure (binOp Sca toValBool o a b)
      _ -> panic "bad plus"
  PrimIdentity -> pure
  PrimIncr ->
    \case
      Sca (Int x) -> pure (Sca (Int (x + 1)))
      _ -> panic "bad incr"
  PrimAbs ->
    \case
      Sca (Int x) -> pure (Sca (Int (abs x)))
      Sca (Float x) -> pure (Sca (Float (abs x)))
      _ -> panic "bad abs"
  PrimShow -> (pure . Sca . Str . render)
  PrimApp ->
    \case
      Rec r
        | Just (VFun ff) <- lkp (LPos 1) r,
          Just aa <- lkp (LPos 2) r ->
          ff aa
      v -> panic ("bad app: " <> render v)

evalInterp :: Tops -> Expr -> SketchInterp -> Expr -> Expr -> Interp
evalInterp tops iInj iHandlers iSum iSide =
  Interp
    { iInj = evalAr tops iInj,
      iSum = evalAr tops iSum,
      iHandlers = Map.fromList [(name, evalAr tops e) | (name, e) <- iHandlers ^. #ars],
      iSide = iSide
    }

evalDecl :: Tops -> Decl -> [(Ident, Top)]
evalDecl tops = \case
  DAr (OFree _ _) name _ e -> [(Lc name, TFreyd e)]
  DAr _ name _ e -> [(Lc name, TFun (evalAr tops e))]
  DInterp name _sketchName iInj iHandlers iSum iSide -> [(Lc name, TInterp (evalInterp tops iInj iHandlers iSum iSide))]
  DOb {} -> []
  DSketch {} -> []
  DCategory cate -> [(Uc (cate ^. #catName), TCat cate)]
  DEffCat effCat -> [(Lc (effCat ^. #effCatStructName), TEffCat effCat)]
  DEff {} -> []
  DEffInterp EffInterp {..} -> [(Uc (UcIdent (interpIn ^. #getUcIdent <> "." <> name)), TExpr e) | (LcIdent name, e) <- handlers] -- TODO: total hack

evalMain :: Val -> Decls -> IO Val
evalMain v ds = eval v ds (Top (LcIdent "main"))

eval :: Val -> Decls -> Expr -> IO Val
eval v ds expr =
  let tops =
        Map.fromList $
          [ (Lc "sumPreserver", TFun pure),
            (Lc "i", TFun pure)
          ]
            ++ [bind | d <- ds, bind <- evalDecl tops d]
   in evalAr tops expr v

jsCall :: Text -> [Text] -> Text
jsCall f xs = f <> "(" <> Text.intercalate "," xs <> ")"

jsCall1 :: Text -> Text -> Text
jsCall1 f x = jsCall f [x]

jsCall2 :: Text -> Text -> Text -> Text
jsCall2 f x y = jsCall f [x, y]

jsCone :: [(Label, Text)] -> Text
jsCone xs = "{" <> Text.intercalate "," [jsLabel lab <> ":" <> f | (lab, f) <- xs] <> "}"

jsLabel :: Label -> Text
jsLabel (LPos i) = show (show i :: Text)
jsLabel (LNam l) = show (render l)

evalJS :: Expr -> Text
evalJS = \case
  Lit x -> jsCall1 "mkConst" (render x)
  Tuple xs -> evalJS (tupleToCone xs)
  EConst x -> jsCall1 "mkConst" (evalJS x)
  Proj p -> labCombi "proj" p
  Inj p -> labCombi "inj" p
  Top t -> labCombi "top" (LNam t)
  Distr p -> labCombi "distr" p
  Comp xs -> foldl' go "identity" xs
    where
      go x e = jsCall2 "comp" x (evalJS e)
  Cone xs -> jsCall1 "cone" $ jsCone [(componentLabel label, evalJS e) | (label, e) <- xs]
  CoCone xs -> jsCall1 "cocone" $ jsCone [(label, evalJS e) | (label, e) <- xs]
  BinOp o f g -> jsCall "binOp" [show (render (PrimOp o)), evalJS f, evalJS g]
  EPrim prim -> jsCall1 "prim" (show (render prim))
  _ -> panic "JS TODO"
  where
    labCombi f p = jsCall1 f (jsLabel p)

mkJS :: Decls -> IO Text
mkJS decls = do
  jsPreludePath <- getDataFileName "js/lawvere.js"
  jsPrelude <- readFile jsPreludePath
  pure $ "console.log(" <> jsPriv (jsPrelude <> " " <> prelude) "tops" <> ".main({}));"
  where
    prelude = statements (mkDecl <$> decls)
    mkDecl (DAr _ (LcIdent name) _ e) = addTop name (evalJS e)
    mkDecl DOb {} = ""
    mkDecl DSketch {} = ""
    mkDecl _ = ""
    addTop name e = "tops[\"" <> name <> "\"] = " <> e
    statements xs = Text.intercalate "\n" ((<> ";") <$> xs)
    jsPriv :: Text -> Text -> Text
    jsPriv x r = "(function(){\n" <> x <> " return " <> r <> ";})()"
