module Lawvere.Eval where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Ob
import Lawvere.Scalar
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

type Tops = Map LcIdent Top

evalAr :: Tops -> Expr -> Fun
evalAr tops = \case
  EPrim _ -> panic "TODO prim"
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
  Top i -> \v -> case Map.lookup i tops of
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
    _ -> panic "bad 3"
  Proj l -> \case
    v@(Rec xs) -> case Map.lookup l xs of
      Just y -> pure y
      Nothing -> panic ("bad record projection, no key: " <> render l <> " in " <> render v)
    _ -> panic ("bad record projection, not record: " <> show l)
  Comp fs -> foldr' comp pure fs
    where
      comp e cur = evalAr tops e >=> cur
  Tuple fs -> evalAr tops (tupleToCone fs)
  Cone fs -> mkCone fs
  CoCone fs -> evalAr tops (Top (LcIdent "sumPreserver")) >=> mkCoCone fs
  EFunApp "io" e ->
    evalAr
      ( Map.fromList
          [ (LcIdent "putLine", TFun lawPutLine),
            (LcIdent "getLine", TFun (const (Sca . Str <$> getLine))),
            ( LcIdent "side",
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
    case Map.lookup name tops of
      Just (TFun ff) -> \x -> do
        let f = VFun (evalAr tops e)
        g <- ff f
        case g of
          VFun g' -> g' x
          v -> panic $ "bad efunapp: " <> render v
      Just (TInterp Interp {..}) ->
        evalAr ((TFun <$> iHandlers) <> Map.fromList [(LcIdent "i", TFun iInj), (LcIdent "sumPreserver", TFun iSum), (LcIdent "side", TExpr iSide)] <> tops) e
      _ -> panic "bad efunapp"
  Curry _ _ -> panic "curry"
  Object _ -> const (pure (VFun pure))
  CanonicalInj e -> evalAr tops (EFunApp (LcIdent "i") e)
  Side lab e -> tr "before sidecar" >=> applyInj (sidePrep (LNam lab)) >=> tr "after prep" >=> sidecar e >=> tr "after side" >=> applyInj (sideUnprep (LNam lab))
  BinOp o f g -> \v -> do
    x <- evalAr tops f v
    y <- evalAr tops g v
    pure (binOp o x y)
  where
    binOp (NumOp o) (Sca (Int x)) (Sca (Int y)) = Sca (Int (numOp o x y))
    binOp (NumOp o) (Sca (Float x)) (Sca (Float y)) = Sca (Float (numOp o x y))
    binOp (CompOp o) (Sca (Int x)) (Sca (Int y)) = compOp o x y
    binOp (CompOp o) (Sca (Float x)) (Sca (Float y)) = compOp o x y
    binOp _ _ _ = panic "bad binop"
    numOp :: (Num a) => NumOp -> a -> a -> a
    numOp OpPlus = (+)
    numOp OpMinus = (-)
    numOp OpTimes = (*)
    compOp o x y = toValBool (compa o x y)
    compa :: (Ord a) => CompOp -> a -> a -> Bool
    compa OpLt = (<)
    compa OpLte = (<=)
    compa OpGt = (>)
    compa OpGte = (>=)
    toValBool True = Tag (LNam "true") (Rec mempty)
    toValBool False = Tag (LNam "false") (Rec mempty)
    lawPutLine = \case
      Sca (Str s) -> do
        putStrLn s
        pure (Rec mempty)
      _ -> panic "bad putLine"
    tr :: Text -> Fun
    tr _t v = do
      -- putStrLn $ "=> TRACE: " <> t
      -- putStrLn (render v)
      -- putStrLn ("--------" :: Text)
      pure v
    sidecar e = case getTop "side" of
      TExpr sideE -> evalAr (Map.insert (LcIdent "eff") (TFun (evalAr tops e)) tops) sideE
      TFun f -> \v -> do
        g <- f (VFun (evalAr tops e))
        case g of
          VFun g' -> g' v
          _ -> panic "bad sidecar"
      _ -> panic "bad sidecar"
    getTop name = case Map.lookup (LcIdent name) tops of
      Just top -> top
      _ -> panic "could not get top"
    getTopFun name = case getTop name of
      TFun f -> f
      _ -> panic "bad getTopFun"
    sidePrep :: Label -> Fun
    sidePrep lab = \case
      Rec r@(Map.lookup lab -> Just x) -> pure (Rec (Map.fromList [(LNam "eff", x), (LNam "pur", Rec (Map.delete lab r))]))
      v -> panic $ "bad side prep: " <> render lab <> " - " <> render v
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
              Nothing -> panic ("bad cocone: " <> render l <> " " <> render x)
            v -> panic ("bad cocone: " <> render v)

lkp :: Label -> Map Label a -> Maybe a
lkp = Map.lookup

primTops :: Tops
primTops =
  Map.fromList
    [ "plus"
        |-> \case
          Rec r
            | Just (Sca (Int x)) <- lkp (LPos 1) r,
              Just (Sca (Int y)) <- lkp (LPos 2) r ->
              pure (Sca (Int (x + y)))
          _ -> panic "bad plus",
      "print"
        |-> \case
          v -> do
            putStrLn ("PRINT" :: Text)
            putStrLn (render v)
            pure (Rec mempty),
      "concat"
        |-> \case
          Rec r
            | Just (Sca (Str x)) <- lkp (LPos 1) r,
              Just (Sca (Str y)) <- lkp (LPos 2) r ->
              pure (Sca (Str (x <> y)))
          _ -> panic "bad concat",
      "incr"
        |-> \case
          Sca (Int x) -> pure (Sca (Int (x + 1)))
          _ -> panic "bad incr",
      "abs"
        |-> \case
          Sca (Int x) -> pure (Sca (Int (abs x)))
          Sca (Float x) -> pure (Sca (Float (abs x)))
          _ -> panic "bad abs",
      "show" |-> (pure . Sca . Str . render),
      "app"
        |-> \case
          Rec r
            | Just (VFun ff) <- lkp (LPos 1) r,
              Just aa <- lkp (LPos 2) r ->
              ff aa
          v -> panic ("bad app: " <> render v),
      -- The base interp:
      "i" |-> pure,
      "sumPreserver" |-> pure
    ]
  where
    x |-> y = (LcIdent x, TFun y)

(=:) :: a -> b -> (a, b)
(=:) = (,)

evalInterp :: Tops -> Expr -> SketchInterp -> Expr -> Expr -> Interp
evalInterp tops iInj iHandlers iSum iSide =
  Interp
    { iInj = evalAr tops iInj,
      iSum = evalAr tops iSum,
      iHandlers = Map.fromList [(name, evalAr tops e) | (name, e) <- iHandlers ^. #ars],
      iSide = iSide
    }

evalDecl :: Tops -> Decl -> [(LcIdent, Top)]
evalDecl tops = \case
  DAr (OFree _ (Extension _ _)) name _ _ e -> [(name, TFreyd e)]
  DAr _ name _ _ e -> [(name, TFun (evalAr tops e))]
  DInterp name _sketchName iInj iHandlers iSum iSide -> [(name, TInterp (evalInterp tops iInj iHandlers iSum iSide))]
  DOb {} -> []
  DSketch {} -> []

evalMain :: Val -> Decls -> IO Val
evalMain v ds = eval v ds (Top (LcIdent "main"))

eval :: Val -> Decls -> Expr -> IO Val
eval v ds expr =
  let tops = primTops <> Map.fromList [bind | d <- ds, bind <- evalDecl tops d]
   in evalAr tops expr v

primsJS :: [(Text, Text)]
primsJS =
  [ "plus" =: "x => x[\"1\"] + x[\"2\"];",
    "print" =: "x => {console.log('PRINT', x);return {};}",
    "incr" =: "x => x+1;",
    "app" =: "x => x[\"1\"](x[\"2\"])"
  ]

jsCall1 :: Text -> Text -> Text
jsCall1 f x = f <> "(" <> x <> ")"

jsCall2 :: Text -> Text -> Text -> Text
jsCall2 f x y = f <> "(" <> x <> "," <> y <> ")"

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
  _ -> panic "TODO"
  where
    labCombi f p = jsCall1 f (jsLabel p)

mkJS :: Decls -> Text
mkJS decls =
  "console.log(" <> jsPriv prelude "tops" <> ".main({}));"
  where
    prelude =
      jsClone
        <> "var tops = {};\n"
        <> statements
          ["let " <> name <> " = " <> body | (name, body) <- jsCombis]
        <> statements (uncurry addTop <$> primsJS)
        <> statements (mkDecl <$> decls)
    mkDecl (DAr _ (LcIdent name) _ _ e) = addTop name (evalJS e)
    mkDecl DOb {} = ""
    mkDecl DSketch {} = ""
    mkDecl _ = ""
    addTop name e = "tops[\"" <> name <> "\"] = " <> e
    statements xs = Text.intercalate "\n" ((<> ";") <$> xs)
    jsPriv :: Text -> Text -> Text
    jsPriv x r = "(function(){\n" <> x <> " return " <> r <> ";})()"
    jsCombis :: [(Text, Text)] =
      [ "identity" =: "x => x",
        "mkConst" =: "function(v){return function(_){ return v;};}",
        "comp" =: "function(f1, f2){ return function(x){ return f2(f1(x)); } }",
        "top" =: "i => { return function(x){ return tops[i](x); };}",
        "proj" =: "i => function(x){ return x[i];}",
        "inj" =: "i => function(x){ return {tag: i, val: x};}",
        "distr"
          =: "l =>\
             \ function(r){\
             \   let new_r = clone(r);\
             \   new_r[l] = r[l].val;\
             \   return {tag: r[l].tag, val: new_r};\
             \}",
        "cone"
          =: "c => function(x){return Object.fromEntries(Object.entries(c).map(([k,f]) => [k,f(x)]));}",
        "cocone" =: "(c) => function(x){return (c[x.tag])(x.val);}"
      ]

jsClone :: Text
jsClone = "function clone(e){if(null===e||\"object\"!=typeof e||\"isActiveClone\"in e)return e;if(e instanceof Date)var n=new e.constructor;else n=e.constructor();for(var t in e)Object.prototype.hasOwnProperty.call(e,t)&&(e.isActiveClone=null,n[t]=clone(e[t]),delete e.isActiveClone);return n}"
