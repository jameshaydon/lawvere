{-# LANGUAGE OverloadedLists #-}

module Lawvere.Check where

import Control.Lens
import Data.Generics.Labels ()
import Data.List (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Ob
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (check)

prims :: Decls -> TcTops
prims decls =
  TcTops
    { obs =
        Map.fromList $
          [ ("Base", OPrim TBase),
            ("Int", OPrim TInt),
            ("Float", OPrim TFloat),
            ("String", OPrim TString)
          ]
            ++ [(name, ob) | DOb _ name ob <- decls],
      ars = Map.fromList [(name, (Scheme [] (Niche a b), e)) | DAr _ name (Niche a b) e <- decls]
    }

primScheme :: Prim -> Scheme
primScheme = \case
  Pfn fn -> case fn of
    PrimIdentity -> [va] .: (ta, ta)
    PrimApp -> [va, vb] .: (OTuple [ta :=> tb, ta], tb)
    PrimIncr -> [] .: (OPrim TInt, OPrim TInt)
    PrimAbs -> [va] .: (ta, ta)
    PrimShow -> [va] .: (ta, OPrim TString)
    PrimConcat -> [] .: (OTuple [OPrim TString, OPrim TString], OPrim TString)
  PrimOp o -> case o of
    CompOp _ -> [va] .: (ta, tBool)
    NumOp _ -> [va] .: (OTuple [ta, ta], ta)
  where
    vs .: (a, b) = Scheme vs (Niche a b)
    va = MkVar 0
    vb = MkVar 1
    ta = OVar va
    tb = OVar vb

inferPrim :: Prim -> Check (Ob, Ob)
inferPrim = refresh . primScheme

checkProg :: Decls -> (Either Err (), [Text])
checkProg decls = runCheck (prims decls) initState (checkDecls decls)

data Err
  = CeCantProjLabelMissing Label DiscDiag
  | CeCantInjLabelMissing Label DiscDiag
  | CeCantProjOutOfNonLim Label (Ob, Ob)
  | CeCantInjIntoNonCoLim Label (Ob, Ob)
  | CeIdOnNonEqObjects Ob Ob
  | CeUndefinedAr LcIdent
  | CeUndefinedOb UcIdent
  | CeCantInfer Expr
  | CeCantUnify Ob Ob
  | CeDistrLabelNotInSource Label DiscDiag
  | CeDistrWasNotColimInSource Label Ob
  | CeDistrSourceNotLim Label Ob
  | CeConstTargetNotArr Ob Expr
  | CeCantInferTarget Ob Expr
  | CeCantInferSource Ob Expr
  | CeCoConeCasesDontMatchColimSource DiscDiag [(Label, Expr)] (Label, Either Expr Ob)
  | CeConeComponentsDontMatchTargetLim DiscDiag [(Label, Expr)] (Label, Either Expr Ob)
  | CeCantInferTargetOfEmptyCoCone
  | CeCantUnifyPairwise (Label, Either Ob Ob)
  | CeCantCheck Ob Ob Expr
  | CeCantApplyFunctor Expr Ob
  | CeEff
  | CeUnCurryMissing Label [(Label, Ob)]
  deriving stock (Show)

instance Disp Err where
  disp = \case
    CeCantInferSource target f ->
      sep ["given target", disp target, "can't infer source of", disp f]
    CeCantInferTarget source f ->
      sep ["given source", disp source, "can't infer target of", disp f]
    CeCantUnify a b ->
      vsep ["Can't unify:", indent 2 (disp a), "and", indent 2 (disp b)]
    CeCantProjLabelMissing label diag ->
      sep ["Can't project", disp label, "as it is missing:", disp (Lim diag)]
    CeCantCheck a b f ->
      sep ["Can't check:", disp a, "to", disp b, ":", disp f]
    CeDistrWasNotColimInSource label source ->
      sep ["Distr was not a colim in source", disp label, disp source]
    CeCantApplyFunctor e o ->
      sep ["Can't apply functor", pretty (show e :: Text), "to", disp o]
    err -> pretty (show err :: Text) -- TODO

newtype Check a = Check
  { runTypecheckM :: ExceptT Err (StateT TcState (Reader TcTops)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader TcTops, MonadState TcState, MonadError Err)

runCheck :: TcTops -> TcState -> Check a -> (Either Err a, [Text])
runCheck init_env init_state =
  over _2 warnings . flip runReader init_env . flip runStateT init_state . runExceptT . runTypecheckM

data TcState = TcState
  { ob_vars :: Map MetaVar Ob,
    nextFresh :: Int,
    warnings :: [Text]
  }
  deriving stock (Generic)

initState :: TcState
initState =
  TcState
    { ob_vars = mempty,
      nextFresh = 0,
      warnings = []
    }

warning :: Text -> Check ()
warning w = #warnings %= (w :)

data TcTops = TcTops
  { ars :: Map LcIdent (Scheme, Expr),
    obs :: Map UcIdent Ob
  }
  deriving stock (Generic)

instance Semigroup TcTops where
  TcTops ars obs <> TcTops ars' obs' =
    TcTops (ars' <> ars) (obs' <> obs)

fresh :: Check MetaVar
fresh = do
  i <- use #nextFresh
  #nextFresh += 1
  pure (MkVar i)

freshT :: Check Ob
freshT = OVar <$> fresh

data Scheme = Scheme (Set MetaVar) (Niche Ob) deriving stock (Show)

instance Disp Scheme where
  disp (Scheme vars (Niche source target)) =
    "forall" <+> parens (hsep (punctuate comma (map (disp . OVar) (Set.toList vars)))) <+> dot <+> disp source <+> "-->" <+> disp target

refresh :: Scheme -> Check (Ob, Ob)
refresh (Scheme vars (Niche source target)) = do
  subst <- Map.fromList <$> forM (Set.toList vars) (\var -> (var,) <$> fresh)
  let repl thing = thing & over freeVars (subst Map.!)
  pure (repl source, repl target)

readMetaObVar :: MetaVar -> Check (Maybe Ob)
readMetaObVar v = use (#ob_vars . at v)

writeMetaObVar :: MetaVar -> Ob -> Check ()
writeMetaObVar v typ | OVar v == typ = pure ()
writeMetaObVar v typ =
  readMetaObVar v >>= \case
    Nothing -> do
      #ob_vars . at v ?= typ
      #ob_vars . each . filtered (== OVar v) .= typ
    Just x -> panic ("Unification variable " <> show v <> " is already assigned to: " <> show x)

getNamedAr :: LcIdent -> Check ((Ob, Ob), Expr)
getNamedAr top = do
  item_ <- view (#ars . at top)
  case item_ of
    Just (scheme, e) -> (,e) <$> refresh scheme
    Nothing -> throwError (CeUndefinedAr top)

getNamedOb :: UcIdent -> Check Ob
getNamedOb name = do
  x <- view (#obs . at name)
  case x of
    Just t -> pure t
    Nothing -> throwError (CeUndefinedOb name)

checkDecl :: Decl -> Check ()
checkDecl (DAr (OFree _ _) _ _ _) = warning "Freyd arrows are not checked yet."
checkDecl (DAr _ _ (Niche a b) body) = check (a, b) body
checkDecl DInterp {} = warning "Interpretatoins are not checked yet."
checkDecl DOb {} = pure () -- TODO
checkDecl DSketch {} = pure ()
checkDecl DCategory {} = warning "Categories are not checked yet."
checkDecl DEffCat {} = warning "Effect category structures are not checked yet."
checkDecl DEff {} = pure () -- TODO
checkDecl DEffInterp {} = warning "Effect interpretations are not checked yet."

checkDecls :: Decls -> Check ()
checkDecls = traverse_ checkDecl

infer :: Expr -> Check (Ob, Ob)
infer = \case
  Comp [] -> do
    a <- freshT
    pure (a, a)
  Comp (f : fs) -> do
    (a, b) <- infer f
    c <- inferTarget b (Comp fs)
    pure (a, c)
  Top f -> fst <$> getNamedAr f
  EConst f -> do
    (a, b) <- infer f
    pure (Lim [], a :=> b)
  f -> throwError (CeCantInfer f)

resolveOb :: Ob -> Check Ob
resolveOb (ONamed name) = getNamedOb name
resolveOb (TFunApp f o) = do
  (_, e) <- getNamedAr f
  applyFunctor e o
resolveOb t = pure t

applyFunctor :: Expr -> Ob -> Check Ob
applyFunctor EId o = pure o
applyFunctor (ELim fs) o = do
  os <- (traverse . _2) (`applyFunctor` o) fs
  pure (Lim os)
applyFunctor (ECoLim fs) o = do
  os <- (traverse . _2) (`applyFunctor` o) fs
  pure (CoLim os)
applyFunctor (Top name) o =
  pure (TFunApp name o)
applyFunctor e o = throwError (CeCantApplyFunctor e o)

scalarTyp :: Sca -> Ob
scalarTyp s = OPrim $ case s of
  Int _ -> TInt
  Str _ -> TString
  Float _ -> TFloat

noEffects :: [(ConeComponent, Expr)] -> Check [(Label, Expr)]
noEffects = traverse go
  where
    go :: (ConeComponent, Expr) -> Check (Label, Expr)
    go (ConeComponent Pure lab, e) = pure (lab, e)
    go _ = throwError CeEff

tBool :: Ob
tBool = CoLim [(LNam "true", Lim []), (LNam "false", Lim [])]

inferTarget :: Ob -> Expr -> Check Ob
inferTarget source EId = pure source
inferTarget a (BinComp f g) = do
  b <- inferTarget a f
  inferTarget b g
inferTarget source (Tuple fs) =
  inferTarget source (tupleToCone fs)
inferTarget a (EPrim p) = do
  (a', b') <- inferPrim p
  unify a a'
  pure b'
inferTarget a (BinOp (NumOp _) f g) = do
  b <- inferTarget a f
  b' <- inferTarget a g
  unify b b'
  pure b
inferTarget a (BinOp (CompOp _) f g) = do
  b <- inferTarget a f
  b' <- inferTarget a g
  unify b b'
  pure tBool
inferTarget a (InterpolatedString ps) = do
  traverse_ go ps
  pure strTyp
  where
    strTyp = ONamed (UcIdent "String")
    go (ISRaw _) = pure ()
    go (ISExpr e) = check (a, strTyp) e
inferTarget (TFunApp name a) (EFunApp name' f)
  | name == name' = do
    b <- inferTarget a f
    pure (TFunApp name b)
inferTarget (TFunApp name o) f = do
  (_, e) <- getNamedAr name
  o' <- applyFunctor e o
  inferTarget o' f
inferTarget a (Top name) = do
  ((a', b), _) <- getNamedAr name
  unify a a'
  pure b
inferTarget (ONamed name) f = do
  source <- getNamedOb name
  inferTarget source f
inferTarget _ (Lit s) = pure (scalarTyp s)
inferTarget _ (Cone []) =
  pure (Lim [])
inferTarget source (Cone fs) = do
  fs' <- noEffects fs
  Lim <$> traverse (_2 (inferTarget source)) fs'
inferTarget (CoLim as) (CoCone fs) =
  case pairwise as fs of
    Right pairs -> do
      bs <- forM pairs $ \(_, (a, f)) -> inferTarget a f
      case bs of
        [] -> throwError CeCantInferTargetOfEmptyCoCone
        b : _ -> do
          unifyMany bs
          pure b
    Left err -> throwError (CeCoConeCasesDontMatchColimSource as fs err)
inferTarget _ (Inj _) = do
  warning "Not checking target of injection."
  freshT
inferTarget (OTuple as) f =
  inferTarget (prodToLim as) f
inferTarget source (Distr label) =
  inferDistrTarget label source
inferTarget (Lim as) (Proj label) =
  lookup label as ?: CeCantProjLabelMissing label as
inferTarget _ (EConst f) = do
  (a, b) <- infer f
  pure (a :=> b)
inferTarget _ (EFunApp _ _) = do
  warning "Functor applications are not checked yet."
  freshT
inferTarget _ (InitInterp _ _) = do
  warning "Initital interpretations are not checked yet."
  freshT
inferTarget source f =
  let f' = desugar f
   in if f == f' then throwError (CeCantInferTarget source f) else inferTarget source f'

inferSource :: Ob -> Expr -> Check Ob
inferSource target EId = pure target
inferSource target (BinComp f g) = do
  b <- inferSource target g
  inferSource b f
inferSource target (Lit s) = do
  unify target (scalarTyp s)
  freshT
inferSource target (Top name) = do
  ((a, b), _) <- getNamedAr name
  unify b target
  pure a
inferSource (TFunApp name b) (EFunApp name' f)
  | name == name' = do
    a <- inferSource b f
    pure (TFunApp name a)
inferSource (TFunApp name o) f = do
  (_, e) <- getNamedAr name
  o' <- applyFunctor e o
  inferTarget o' f
inferSource (ONamed name) f = do
  target <- getNamedOb name
  inferSource target f
inferSource (OTuple as) f = inferSource (prodToLim as) f
inferSource target (Tuple fs) = inferSource target (tupleToCone fs)
inferSource target (Cone []) = do
  unify target (Lim [])
  freshT
inferSource (Lim bs) (Cone fs) = do
  fs' <- noEffects fs
  case pairwise bs fs' of
    Left unmatch -> throwError (CeConeComponentsDontMatchTargetLim bs fs' unmatch)
    Right pairs -> do
      let go (_, (b, f)) = inferSource b f
      as <- traverse go pairs
      case as of
        [] -> freshT
        a : _ -> do
          unifyMany as
          pure a
inferSource target (CoCone fs) = do
  diag <- traverse (_2 (inferSource target)) fs
  pure (CoLim diag)
inferSource (CoLim as) (Inj lab) = case lookup lab as of
  Just a -> pure a
  Nothing -> throwError (CeCantInjLabelMissing lab as)
inferSource _ (Proj _) = do
  warning "Not checking source of projection."
  freshT
inferSource _ (Distr _) = do
  warning "Not checking source of distributor."
  freshT
inferSource target f =
  let f' = desugar f
   in if f == f' then throwError (CeCantInferSource target f) else inferSource target f'

inferDistrTarget :: Label -> Ob -> Check Ob
inferDistrTarget label (Lim theLim) = do
  (labelColim, xs) <- lookupRest label theLim ?: CeDistrLabelNotInSource label theLim
  labelColim' <- resolveOb labelColim
  as <- labelColim' ^? #_CoLim ?: CeDistrWasNotColimInSource label labelColim'
  pure $ CoLim [(l, Lim ((label, a) : xs)) | (l, a) <- as]
inferDistrTarget label source = throwError (CeDistrSourceNotLim label source)

check :: (Ob, Ob) -> Expr -> Check ()
check (a, b) EId = unify a b
check (a, c) (BinComp f g) = do
  b <- inferTarget a f
  check (b, c) g
check (a, b) (EPrim p) = do
  (a', b') <- inferPrim p
  unify a a'
  unify b b'
check (a, b) (BinOp (NumOp _) f g) = do
  warning "Not checking Num instance."
  check (a, b) f
  check (a, b) g
check (a, b) (BinOp (CompOp _) f g) = do
  unify b tBool
  c <- inferTarget a f
  c' <- inferTarget a g
  unify c c'
  warning "Not checking Ord instance."
check (a, b) (InterpolatedString ps) = do
  unify b strTyp
  traverse_ go ps
  where
    strTyp = ONamed (UcIdent "String")
    go (ISRaw _) = pure ()
    go (ISExpr e) = check (a, strTyp) e
check (ONamed name, b) f = do
  a <- getNamedOb name
  check (a, b) f
check (a, ONamed name) f = do
  b <- getNamedOb name
  check (a, b) f
check (a, b) (Top name) = do
  ((a', b'), _) <- getNamedAr name
  unify a a'
  unify b b'
check (a, b) (ELim fs) =
  forM_ fs $ \(_, f) -> check (a, b) f
check (a, b) (ECoLim fs) =
  forM_ fs $ \(_, f) -> check (a, b) f
check (_, b :=> c) (EConst f) = check (b, c) f
check (_, b) (EConst f) = throwError (CeConstTargetNotArr b f)
check (_, b) (Lit s) = unify b (scalarTyp s)
check (Lim as, b) (Proj label) = do
  a <- lookup label as ?: CeCantProjLabelMissing label as
  unify a b
check (OVar _, _) (Proj _) = warning "Can't add component constraint."
check (OTuple as, b) f = check (prodToLim as, b) f
check (a, OTuple bs) f = check (a, prodToLim bs) f
check (a, CoLim bs) (Inj label) = do
  b <- lookup label bs ?: CeCantInjLabelMissing label bs
  unify b a
check (a, b) (Inj lab) = do
  b' <- resolveOb b
  check (a, b') (Inj lab)
check niche (Tuple fs) = check niche (tupleToCone fs)
check (a, Lim bs) (Cone fs) = do
  fs' <- noEffects fs
  case pairwise bs fs' of
    Left unmatch -> throwError (CeConeComponentsDontMatchTargetLim bs fs' unmatch)
    Right pairs -> do
      let go (_, (b, f)) = check (a, b) f
      traverse_ go pairs
check (CoLim as, b) (CoCone fs) = case pairwise as fs of
  Left unmatch -> throwError (CeCoConeCasesDontMatchColimSource as fs unmatch)
  Right pairs -> do
    let go (_, (a, f)) = check (a, b) f
    traverse_ go pairs
check (a, b) (Distr label) = do
  b' <- inferDistrTarget label a
  unify b' b
check (TFunApp name a, TFunApp name' b) (EFunApp name'' f)
  | name == name', name == name'' = check (a, b) f
check (a@(TFunApp _ _), b) f = do
  a' <- resolveOb a
  check (a', b) f
check _ (EFunApp _ _) = warning "Functor applications are not checked yet."
check _ (InitInterp _ _) = warning "Initial interpretations are not checked yet."
check _ (FromInit _ _) = warning "Initial interpretations are not checked yet."
check _ (ESketchInterp _) = warning "Sketch interpretations are not checked yet."
check (Lim diag, b) (UnCurry lbl f) = case lookup lbl diag of
  Just a -> check (Lim (filter ((== lbl) . fst) diag), a :=> b) f
  _ -> throwError (CeUnCurryMissing lbl diag)
check (Lim diag, a :=> b) (Curry lbl f) = check (Lim ((lbl, a) : diag), b) f
check (Lim diag, b) (Fix lbl f) = check (Lim ((lbl, b) : diag), b) f
check (a, b) f =
  let f' = desugar f
   in if f == f'
        then throwError (CeCantCheck a b f)
        else check (a, b) f'

unify :: Ob -> Ob -> Check ()
unify (ONamed name) (ONamed name') | name == name' = pure ()
unify (ONamed name) a = do
  t <- getNamedOb name
  unify t a
unify a (ONamed name) = do
  t <- getNamedOb name
  unify t a
unify a@(OPrim p) b@(OPrim p')
  | p == p' = pure ()
  | otherwise = throwError (CeCantUnify a b)
unify (OVar u) (OVar v) = do
  u' <- readMetaObVar u
  v' <- readMetaObVar v
  case (u', v') of
    (Nothing, Nothing) -> writeMetaObVar u (OVar v)
    (Just u'', Nothing) -> unify u'' (OVar v)
    (Just u'', Just v'') -> unify u'' v''
    (Nothing, Just v'') -> unify (OVar u) v''
unify (OVar v) typ =
  readMetaObVar v >>= \case
    Nothing -> writeMetaObVar v typ
    Just r -> unify r typ
unify typ (OVar v) =
  readMetaObVar v >>= \case
    Nothing -> writeMetaObVar v typ
    Just r -> unify typ r
unify (OTuple as) b = unify (prodToLim as) b
unify a (OTuple bs) = unify a (prodToLim bs)
unify (Lim as) (Lim bs) = do
  ys <- pairwise as bs ?:: CeCantUnifyPairwise
  forM_ ys $ \(_, (a, b)) -> unify a b
unify (CoLim as) (CoLim bs) = do
  ys <- pairwise as bs ?:: CeCantUnifyPairwise
  forM_ ys $ \(_, (a, b)) -> unify a b
unify (a :=> b) (a' :=> b') = unify a a' >> unify b b'
unify (TFunApp name a) (TFunApp name' b)
  | name == name' -- TODO: we should also handle the non-equal case
    =
    unify a b
unify a@(TFunApp _ _) b = do
  a' <- resolveOb a
  unify a' b
unify a b@(TFunApp _ _) = do
  b' <- resolveOb b
  unify a b'
unify a b = throwError (CeCantUnify a b)

unifyMany :: [Ob] -> Check ()
unifyMany (a : b : rest) = unify a b >> unifyMany (b : rest)
unifyMany _ = pure ()
