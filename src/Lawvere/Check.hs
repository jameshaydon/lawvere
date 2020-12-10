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
      ars =
        Map.fromList $
          [ ("plus", (Scheme [] (OTuple [OPrim TInt, OPrim TInt], ONamed "Int"), EPrim PrimPlus)),
            ("incr", (Scheme [] (OPrim TInt, OPrim TInt), EPrim PrimIncr)),
            ("app", (Scheme [va, vb] (OTuple [ta :=> tb, ta], tb), EPrim PrimApp))
          ]
            ++ [(name, (Scheme [] (a, b), e)) | DAr _ name a b e <- decls]
    }
  where
    va = MkVar 0
    vb = MkVar 1
    ta = OVar va
    tb = OVar vb

checkProg :: Decls -> Either Err ()
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
  | CeCantInferTargetOfEmptyCoCone
  | CeCantUnifyPairwise (Label, Either Ob Ob)
  | CeCantCheck Ob Ob Expr
  | CeCantApplyFunctor Expr Ob
  deriving stock (Show)

instance Disp Err where
  disp = \case
    CeCantInferSource target f ->
      sep ["given target", disp target, "can't infer source of", disp f]
    CeCantInferTarget source f ->
      sep ["given source", disp source, "can't infer target of", disp f]
    CeCantUnify a b ->
      sep ["Can't unify:", disp a, "and", disp b]
    CeCantProjLabelMissing label diag ->
      sep ["Can't project", disp label, "as it is missing:", disp (Lim diag)]
    CeCantCheck a b f ->
      sep ["Can't check:", disp a, "to", disp b, "thing:", disp f]
    CeDistrWasNotColimInSource label source ->
      sep ["Distr was not a colim in source", disp label, disp source]
    CeCantApplyFunctor e o ->
      sep ["Can't apply functor", disp e, "to", disp o]
    err -> pretty (show err :: Text) -- TODO

newtype Check a = Check
  { runTypecheckM :: ExceptT Err (StateT TcState (Reader TcTops)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader TcTops,
      MonadState TcState,
      MonadError Err
    )

runCheck :: TcTops -> TcState -> Check a -> Either Err a
runCheck init_env init_state =
  flip runReader init_env . flip evalStateT init_state . runExceptT . runTypecheckM

data TcState = TcState
  { ob_vars :: Map MetaVar Ob,
    nextFresh :: Int
  }
  deriving stock (Generic)

initState :: TcState
initState =
  TcState
    { ob_vars = mempty,
      nextFresh = 0
    }

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

data Scheme = Scheme (Set MetaVar) (Ob, Ob)

instance Disp Scheme where
  disp (Scheme vars (source, target)) =
    "forall" <+> parens (hsep (punctuate comma (map (disp . OVar) (Set.toList vars)))) <+> dot <+> disp source <+> "-->" <+> disp target

refresh :: Scheme -> Check (Ob, Ob)
refresh (Scheme vars (source, target)) = do
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
checkDecl (DAr _ _ a b body) = check (a, b) body
checkDecl DOb {} = pure () -- TODO
checkDecl DSketch {} = pure ()

checkDecls :: Decls -> Check ()
checkDecls = traverse_ checkDecl

infer :: Expr -> Check (Ob, Ob)
infer = \case
  -- Cone fs -> do
  --   a <- freshT
  --   let go (label, f) = do
  --         (a', b) <- infer f
  --         unify a a'
  --         pure (label, b)
  --   bs <- traverse go fs
  --   pure (a, Lim bs)
  -- CoCone fs -> do
  --   b <- freshT
  --   let go (label, f) = do
  --         (a, b') <- infer f
  --         unify b b'
  --         pure (label, a)
  --   as <- traverse go fs
  --   pure (CoLim as, b)
  -- Tuple fs -> infer (Cone (tupleToCone fs))
  -- Lit (Int _) -> (,ONamed "Int") <$> freshT
  -- Lit (Float _) -> (,ONamed "Float") <$> freshT
  -- Lit (Str _) -> (,ONamed "String") <$> freshT
  -- Proj label -> do
  --   b <- freshT
  --   pure (Lim [(label, b)], b)
  -- Inj label -> do
  --   a <- freshT
  --   pure (a, CoLim [(label, a)])
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
applyFunctor (Comp []) o = pure o
applyFunctor (Comp [ELim fs]) o = do
  os <- (traverse . _2) (`applyFunctor` o) fs
  pure (Lim os)
applyFunctor (Comp [ECoLim fs]) o = do
  os <- (traverse . _2) (`applyFunctor` o) fs
  pure (CoLim os)
applyFunctor (Comp [Top name]) o =
  pure (TFunApp name o)
applyFunctor e o = throwError (CeCantApplyFunctor e o)

scalarTyp :: Sca -> Ob
scalarTyp s = OPrim $ case s of
  Int _ -> TInt
  Str _ -> TString
  Float _ -> TFloat

inferTarget :: Ob -> Expr -> Check Ob
inferTarget (TFunApp name a) (EFunApp name' f)
  | name == name' = do
    b <- inferTarget a f
    pure (TFunApp name b)
inferTarget (TFunApp name o) f = do
  (_, e) <- getNamedAr name
  o' <- applyFunctor e o
  inferTarget o' f
inferTarget _ (Top name) = do
  ((_, b), _) <- getNamedAr name
  pure b
inferTarget (ONamed name) f = do
  source <- getNamedOb name
  inferTarget source f
inferTarget _ (Lit s) = pure (scalarTyp s)
inferTarget source (Comp []) = pure source
inferTarget a (Comp (f : fs)) = do
  b <- inferTarget a f
  inferTarget b (Comp fs)
inferTarget source (Tuple fs) =
  inferTarget source (Cone (tupleToCone fs))
inferTarget _ (Cone []) =
  pure (Lim [])
inferTarget source (Cone fs) =
  Lim <$> traverse (_2 (inferTarget source)) fs
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
inferTarget _ (Inj _) =
  freshT
inferTarget (OTuple as) f =
  inferTarget (Lim (tupleToCone as)) f
inferTarget source (Distr label) =
  inferDistrTarget label source
inferTarget (Lim as) (Proj label) =
  lookup label as ?: CeCantProjLabelMissing label as
-- TODO: here we could call yet another function 'infer', since in some cases
-- the full type of 'f' is inferrable.
inferTarget _ (EConst f) = do
  (a, b) <- infer f
  pure (a :=> b)
inferTarget source f = throwError (CeCantInferTarget source f)

inferSource :: Ob -> Expr -> Check Ob
inferSource target (Top name) = do
  ((a, b), _) <- getNamedAr name
  unify b target
  pure a
inferSource (ONamed name) f = do
  target <- getNamedOb name
  inferSource target f
inferSource target (Comp []) = pure target
inferSource target (Comp (f : fs)) = do
  b <- inferSource target (Comp fs)
  inferSource b f
inferSource target (Cone []) = do
  unify target (Lim [])
  freshT
--inferSource target (CoCone fs) = _
inferSource target f = throwError (CeCantInferSource target f)

inferDistrTarget :: Label -> Ob -> Check Ob
inferDistrTarget label (Lim theLim) = do
  (labelColim, xs) <- lookupRest label theLim ?: CeDistrLabelNotInSource label theLim
  labelColim' <- resolveOb labelColim
  as <- labelColim' ^? #_CoLim ?: CeDistrWasNotColimInSource label labelColim'
  pure $ CoLim [(l, Lim ((label, a) : xs)) | (l, a) <- as]
inferDistrTarget label source = throwError (CeDistrSourceNotLim label source)

check :: (Ob, Ob) -> Expr -> Check ()
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
check (a, b) (Comp [ELim fs]) =
  forM_ fs $ \(_, f) -> check (a, b) f
check (a, b) (Comp [ECoLim fs]) =
  forM_ fs $ \(_, f) -> check (a, b) f
check (_, b :=> c) (EConst f) = check (b, c) f
check (_, b) (EConst f) = throwError (CeConstTargetNotArr b f)
check (_, b) (Lit s) = unify b (scalarTyp s)
check (Lim as, b) (Proj label) = do
  a <- lookup label as ?: CeCantProjLabelMissing label as
  unify a b
check niche (Proj label) = throwError (CeCantProjOutOfNonLim label niche)
check (a, CoLim bs) (Inj label) = do
  b <- lookup label bs ?: CeCantInjLabelMissing label bs
  unify b a
check niche (Inj label) = throwError (CeCantInjIntoNonCoLim label niche)
check niche (Tuple fs) = check niche (Cone (tupleToCone fs))
check (a, b) (Comp []) = unify a b
check (a, c) (Comp (f : fs)) = do
  b <- inferTarget a f
  check (b, c) (Comp fs)
check (a, b) (Cone fs) = do
  bs <- traverse (_2 (inferTarget a)) fs
  unify b (Lim bs)
check (a, b) (CoCone fs) = do
  as <- traverse (_2 (inferSource b)) fs
  unify a (CoLim as)
check (a, b) (Distr label) = do
  b' <- inferDistrTarget label a
  unify b' b
check (a, b) f = throwError (CeCantCheck a b f)

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
unify (OTuple as) b = unify (Lim (tupleToCone as)) b
unify a (OTuple bs) = unify a (Lim (tupleToCone bs))
unify (Lim diag) (Lim diag') = unifyLim diag diag'
unify (CoLim as) (CoLim bs) = do
  ys <- pairwise as bs ?:: CeCantUnifyPairwise
  forM_ ys $ \(_, (a, b)) -> unify a b
unify (TFunApp name a) (TFunApp name' b)
  | name == name' -- TODO: we should also handle the non-equal case
    =
    unify a b
unify a b = throwError (CeCantUnify a b)

unifyMany :: [Ob] -> Check ()
unifyMany (a : b : rest) = unify a b >> unifyMany (b : rest)
unifyMany _ = pure ()

unifyLim :: DiscDiag -> DiscDiag -> Check ()
unifyLim _ _ = pure () -- TODO
