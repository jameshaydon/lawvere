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
import Protolude hiding (to)

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
    Tag t v -> hang 2 $ parens (sep [annotate AnCons (disp t <> "."), disp v])
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
  | TSketch Sketch

type Tops = Map Ident Top

evalAr :: Tops -> Expr -> Fun
evalAr tops = \case
  EId -> pure
  BinComp f g -> evalAr tops f >=> evalAr tops g
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
  Cone fs -> mkCone fs
  CoCone fs -> evalAr tops (Top (LcIdent "sumPreserver")) >=> mkCoCone fs
  InitInterp sketchName e -> case Map.lookup (Uc sketchName) tops of
    Just (TSketch Sketch {..}) ->
      evalAr (Map.fromList [(Lc (ar ^. #name), TFun (pure . Tag (LNam (LcIdent (getUcIdent name <> "." <> ar ^. #name . to getLcIdent))))) | ar <- ars] <> tops) e
    _ -> panic "bad init interp"
  EFunApp "io" e ->
    evalAr
      ( Map.fromList
          [ (Lc "putLine", TFun lawPutLine),
            (Lc "getLine", TFun (const (Sca . Str <$> getLine))),
            ( Lc "side",
              TExpr
                ( Cone
                    [ (purNam "pur", Proj (LNam (LcIdent "pur"))),
                      (purNam "eff", Comp [Proj (LNam (LcIdent "eff")), Top "eff"])
                    ]
                )
            )
          ]
          <> tops
      )
      e
  EFunApp "from_init" (Top name) -> case Map.lookup (Lc name) tops of
    Just (TExpr e) -> evalAr tops (EFunApp "from_init" e)
    _ -> panic "bad efun app"
  EFunApp "from_init" (ESketchInterp sketchInterp) ->
    let skName = getUcIdent (sketchInterp ^. #sketchName)
        interpAs = [(LcIdent (skName <> "." <> getLcIdent arName), evalAr tops arExpr) | (arName, arExpr) <- sketchInterp ^. #ars]
        interp = \case
          Tag (LNam i) v
            | Just combi <-
                lookup i interpAs -> do
              v' <- interp v
              combi v'
          Rec r -> Rec <$> traverse interp r
          v -> pure v
     in interp
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
      Just (TExpr e') -> case e' of
        ESketchInterp sketchInterp ->
          let interpAs = [(Lc arName, TFun (evalAr tops arExpr)) | (arName, arExpr) <- sketchInterp ^. #ars]
           in evalAr (Map.fromList interpAs <> tops) e
        _ -> panic "bad efunapp"
      _ -> panic $ "bad efunapp: " <> render name <> " - " <> render e
  -- Curry _ _ -> panic "curry"
  Object _ -> const (pure (VFun pure))
  CanonicalInj e -> evalAr tops (EFunApp (LcIdent "i") e)
  Side lab e -> tr ("before sidecar: " <> render lab) >=> applyInj (sidePrep (LNam lab)) >=> tr "after prep" >=> sidecar e >=> tr "after side" >=> applyInj (sideUnprep (LNam lab))
  SumInjLabelVar _ -> panic "label var remains"
  SumUniCoconeVar _ -> panic "cocone var remains"
  SidePrep lab -> sidePrep lab
  SideUnprep lab -> sideUnprep lab
  e ->
    let e' = desugar e
     in if e == e'
          then panic $ "Unhandled case: " <> show e
          else evalAr tops (desugar e)
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

binFn :: Applicative f => (Val -> Val -> a) -> Val -> f a
binFn f = \case
  Rec r
    | Just x <- lkp (LPos 1) r,
      Just y <- lkp (LPos 2) r ->
      pure (f x y)
  _ -> panic "bad bin fn"

scaBinFn :: Applicative f => (Sca -> Sca -> a) -> Val -> f a
scaBinFn f = binFn $ \x y -> case (x, y) of
  (Sca a, Sca b) -> f a b
  _ -> panic "bad scalar bin fn"

evalPrim :: Prim -> Fun
evalPrim = \case
  PrimOp o -> scaBinFn (binOp Sca toValBool o)
  Pfn pfn -> case pfn of
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
    PrimShow -> pure . Sca . Str . render
    PrimConcat -> scaBinFn $ \x y -> case (x, y) of
      (Str a, Str b) -> Sca (Str (a <> b))
      _ -> panic "bad concat"
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
  DAr (ONamed "Cart") name _ e -> [(Lc name, TExpr e)]
  DAr _ name _ e -> [(Lc name, TFun (evalAr tops e))]
  DInterp name _sketchName iInj iHandlers iSum iSide -> [(Lc name, TInterp (evalInterp tops iInj iHandlers iSum iSide))]
  DOb {} -> []
  DSketch sk -> [(Uc (sk ^. #name), TSketch sk)]
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
  EId -> "identity"
  BinComp f g -> jsCall2 "comp" (evalJS f) (evalJS g)
  Lit x -> jsCall1 "mkConst" (render x)
  EConst x -> jsCall1 "mkConst" (evalJS x)
  Proj p -> labCombi "proj" p
  Inj p -> labCombi "inj" p
  Top t -> labCombi "top" (LNam t)
  Distr p -> labCombi "distr" p
  Cone xs -> jsCall1 "cone" $ jsCone [(componentLabel label, evalJS e) | (label, e) <- xs]
  CoCone xs -> jsCall1 "cocone" $ jsCone [(label, evalJS e) | (label, e) <- xs]
  EPrim prim -> jsCall1 "prim" (show (render prim))
  e ->
    let e' = desugar e
     in if e == e'
          then panic $ "JS TODO: " <> render e
          else evalJS e'
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
