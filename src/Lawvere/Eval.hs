module Lawvere.Eval where

import Data.Bifunctor
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Scalar
import Prettyprinter
import Protolude

data Val
  = Rec (Map Label Val)
  | Tag Label Val
  | Sca Sca
  | VFun (Val -> IO Val)

instance Disp Val where
  disp = \case
    Sca s -> disp s
    Rec r -> commaBrace '=' (Map.toList r)
    Tag t v -> disp t <> "." <> disp v
    VFun _ -> "<unshowable>"

type Tops = Map LcIdent (Val -> IO Val)

evalAr :: Tops -> Expr -> Val -> IO Val
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
                    _ -> panic "bad ELim"
                Nothing -> panic "bad ELim"
          g _ = panic "bad ELim"
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
    Just f -> f v
    Nothing -> panic $ "no toplevel: " <> show i
  Lit x -> const (pure (Sca x))
  Inj i -> pure . Tag i
  Distr l -> \case
    Rec r -> case Map.lookup l r of
      Just y -> case y of
        Tag t z -> pure (Tag t (Rec (Map.insert l z r)))
        _ -> panic "bad1"
      Nothing -> panic "bad2"
    _ -> panic "bad 3"
  Proj l -> \case
    v@(Rec xs) -> case Map.lookup l xs of
      Just y -> pure y
      Nothing -> panic ("bad record projection, no key: " <> show l <> " " <> render v)
    _ -> panic ("bad record projection, not record: " <> show l)
  Comp fs -> foldr' comp pure fs
    where
      comp e cur = evalAr tops e >=> cur
  Tuple parts -> evalAr tops (tupleToCone parts)
  Cone fs -> mkCone fs
  CoCone fs -> mkCoCone fs
  EFunApp name e ->
    let f = VFun (evalAr tops e)
     in \x ->
          case Map.lookup name tops of
            Just ff -> do
              g <- ff f
              case g of
                VFun g' -> g' x
                v -> panic $ "bad efunapp: " <> render v
            Nothing -> panic $ "bad efunapp: " <> render name
  FromFree sketchName overThis withThis -> \x -> panic "TODO from free"
  Curry _ _ -> panic "curry"
  Object _ -> panic "object"
  where
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
              Nothing -> panic ("bad cocone: " <> show l <> " " <> render x)
            v -> panic ("bad cocone: " <> render v)

lkp :: Label -> Map Label a -> Maybe a
lkp = Map.lookup

primTops :: Tops
primTops =
  Map.fromList
    [ LcIdent "plus"
        =: \case
          Rec r
            | Just (Sca (Int x)) <- lkp (LPos 1) r,
              Just (Sca (Int y)) <- lkp (LPos 2) r ->
              pure (Sca (Int (x + y)))
          _ -> panic "bad plus",
      LcIdent "print"
        =: \case
          v -> do
            putStrLn ("PRINT" :: Text)
            putStrLn (render v)
            pure (Rec mempty),
      LcIdent "incr"
        =: \case
          Sca (Int x) -> pure (Sca (Int (x + 1)))
          _ -> panic "bad incr",
      LcIdent "app"
        =: \case
          Rec r
            | Just (VFun ff) <- lkp (LPos 1) r,
              Just aa <- lkp (LPos 2) r ->
              ff aa
          v -> panic ("bad app: " <> render v)
    ]

(=:) :: a -> b -> (a, b)
(=:) = (,)

evalDecl :: Tops -> Decl -> [(LcIdent, Val -> IO Val)]
evalDecl tops = \case
  DAr _ name _ _ e -> [(name, evalAr tops e)]
  DOb {} -> []
  DSketch {} -> []

eval :: Val -> Decls -> IO Val
eval v ds =
  let tops = primTops <> Map.fromList [bind | d <- ds, bind <- evalDecl tops d]
   in case Map.lookup (LcIdent "main") tops of
        Just m -> m v
        Nothing -> panic "No main!"

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
  FromFree {} -> panic "TODO"
  Curry {} -> panic "TODO"
  Object {} -> panic "TODO"
  EFunApp _ _ -> panic "TODO"
  EPrim _ -> panic "TODO"
  ELim _ -> panic "TODO"
  ECoLim _ -> panic "TODO"
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
  where
    labCombi f p = jsCall1 f (jsLabel p)

mkJS :: Decls -> Text
mkJS decls =
  jsPriv prelude "tops"
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
