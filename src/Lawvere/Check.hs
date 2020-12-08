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
import Lawvere.Scalar
import Lawvere.Typ
import Prettyprinter
import Protolude hiding (check)

prims :: Decls -> TcTops
prims decls =
  TcTops
    { obs =
        Map.fromList $
          [ ("Int", TPrim TInt),
            ("Float", TPrim TFloat),
            ("String", TPrim TString)
          ]
            ++ [(name, ob) | DOb name ob <- decls],
      ars =
        Map.fromList $
          [ ("plus", Scheme [] (TTuple [TPrim TInt, TPrim TInt], TNamed "Int")),
            ("incr", Scheme [] (TPrim TInt, TPrim TInt)),
            ("app", Scheme [va, vb] (TTuple [ta :=> tb, ta], tb))
          ]
            ++ [(name, Scheme [] (a, b)) | DAr name a b _ <- decls]
    }
  where
    va = MkVar 0
    vb = MkVar 1
    ta = TVar va
    tb = TVar vb

checkProg :: Decls -> Either Err ()
checkProg decls = runCheck (prims decls) initState (checkDecls decls)

data Err
  = CeCantProjLabelMissing Label DiscDiag
  | CeCantInjLabelMissing Label DiscDiag
  | CeCantProjOutOfNonLim Label (Typ, Typ)
  | CeCantInjIntoNonCoLim Label (Typ, Typ)
  | CeIdOnNonEqObjects Typ Typ
  | CeUndefinedAr LcIdent
  | CeUndefinedOb UcIdent
  | CeCantInfer Expr
  | CeCantUnify Typ Typ
  | CeDistrLabelNotInSource Label DiscDiag
  | CeDistrWasNotColimInSource Label Typ
  | CeDistrSourceNotLim Label Typ
  | CeConstTargetNotArr Typ Expr
  | CeCantInferTarget Typ Expr
  | CeCantInferSource Typ Expr
  | CeCoConeCasesDontMatchColimSource DiscDiag [(Label, Expr)] (Label, Either Expr Typ)
  | CeCantInferTargetOfEmptyCoCone
  | CeCantUnifyPairwise (Label, Either Typ Typ)
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
  { ob_vars :: Map MetaVar Typ,
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
  { ars :: Map LcIdent Scheme,
    obs :: Map UcIdent Typ
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

freshT :: Check Typ
freshT = TVar <$> fresh

data Scheme = Scheme (Set MetaVar) (Typ, Typ)

instance Disp Scheme where
  disp (Scheme vars (source, target)) =
    "forall" <+> parens (hsep (punctuate comma (map (disp . TVar) (Set.toList vars)))) <+> dot <+> disp source <+> "-->" <+> disp target

refresh :: Scheme -> Check (Typ, Typ)
refresh (Scheme vars (source, target)) = do
  subst <- Map.fromList <$> forM (Set.toList vars) (\var -> (var,) <$> fresh)
  let repl thing = thing & over freeVars (subst Map.!)
  pure (repl source, repl target)

readMetaObVar :: MetaVar -> Check (Maybe Typ)
readMetaObVar v = use (#ob_vars . at v)

writeMetaObVar :: MetaVar -> Typ -> Check ()
writeMetaObVar v typ | TVar v == typ = pure ()
writeMetaObVar v typ =
  readMetaObVar v >>= \case
    Nothing -> do
      #ob_vars . at v ?= typ
      #ob_vars . each . filtered (== TVar v) .= typ
    Just x -> panic ("Unification variable " <> show v <> " is already assigned to: " <> show x)

getNamedAr :: LcIdent -> Check (Typ, Typ)
getNamedAr top = do
  scheme_ <- view (#ars . at top)
  case scheme_ of
    Just scheme -> refresh scheme
    Nothing -> throwError (CeUndefinedAr top)

getNamedOb :: UcIdent -> Check Typ
getNamedOb name = do
  x <- view (#obs . at name)
  case x of
    Just t -> pure t
    Nothing -> throwError (CeUndefinedOb name)

checkDecl :: Decl -> Check ()
checkDecl (DAr _ a b body) = check (a, b) body
checkDecl (DOb _ _) = pure () -- TODO

checkDecls :: Decls -> Check ()
checkDecls = traverse_ checkDecl

infer :: Expr -> Check (Typ, Typ)
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
  -- Lit (Int _) -> (,TNamed "Int") <$> freshT
  -- Lit (Float _) -> (,TNamed "Float") <$> freshT
  -- Lit (Str _) -> (,TNamed "String") <$> freshT
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
  Top f -> getNamedAr f
  EConst f -> do
    (a, b) <- infer f
    pure (Lim [], a :=> b)
  f -> throwError (CeCantInfer f)

resolveObName :: Typ -> Check Typ
resolveObName (TNamed name) = getNamedOb name
resolveObName t = pure t

inferTarget :: Typ -> Expr -> Check Typ
inferTarget _ (Lit s) = pure $
  TPrim $ case s of
    Int _ -> TInt
    Str _ -> TString
    Float _ -> TFloat
inferTarget _ (Top name) = do
  (_, b) <- getNamedAr name
  pure b
inferTarget (TNamed name) f = do
  source <- getNamedOb name
  inferTarget source f
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
inferTarget (TTuple as) f =
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

inferSource :: Typ -> Expr -> Check Typ
inferSource target (Top name) = do
  (a, b) <- getNamedAr name
  unify b target
  pure a
inferSource (TNamed name) f = do
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

inferDistrTarget :: Label -> Typ -> Check Typ
inferDistrTarget label (Lim theLim) = do
  (labelColim, xs) <- lookupRest label theLim ?: CeDistrLabelNotInSource label theLim
  labelColim' <- resolveObName labelColim
  as <- labelColim' ^? #_CoLim ?: CeDistrWasNotColimInSource label labelColim'
  pure $ CoLim [(l, Lim ((label, a) : xs)) | (l, a) <- as]
inferDistrTarget label source = throwError (CeDistrSourceNotLim label source)

check :: (Typ, Typ) -> Expr -> Check ()
check (TNamed name, b) f = do
  a <- getNamedOb name
  check (a, b) f
check (a, TNamed name) f = do
  b <- getNamedOb name
  check (a, b) f
check (a, b) (Top name) = do
  (a', b') <- getNamedAr name
  unify a a'
  unify b b'
check (_, b :=> c) (EConst f) = check (b, c) f
check (_, b) (EConst f) = throwError (CeConstTargetNotArr b f)
check (_, b) (Lit s) = unify b $ case s of
  Int _ -> TPrim TInt
  Float _ -> TPrim TFloat
  Str _ -> TPrim TString
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

unify :: Typ -> Typ -> Check ()
unify (TNamed name) (TNamed name') | name == name' = pure ()
unify (TNamed name) a = do
  t <- getNamedOb name
  unify t a
unify a (TNamed name) = do
  t <- getNamedOb name
  unify t a
unify a@(TPrim p) b@(TPrim p')
  | p == p' = pure ()
  | otherwise = throwError (CeCantUnify a b)
unify (TVar u) (TVar v) = do
  u' <- readMetaObVar u
  v' <- readMetaObVar v
  case (u', v') of
    (Nothing, Nothing) -> writeMetaObVar u (TVar v)
    (Just u'', Nothing) -> unify u'' (TVar v)
    (Just u'', Just v'') -> unify u'' v''
    (Nothing, Just v'') -> unify (TVar u) v''
unify (TVar v) typ =
  readMetaObVar v >>= \case
    Nothing -> writeMetaObVar v typ
    Just r -> unify r typ
unify typ (TVar v) =
  readMetaObVar v >>= \case
    Nothing -> writeMetaObVar v typ
    Just r -> unify typ r
unify (TTuple as) b = unify (Lim (tupleToCone as)) b
unify a (TTuple bs) = unify a (Lim (tupleToCone bs))
unify (Lim diag) (Lim diag') = unifyLim diag diag'
unify (CoLim as) (CoLim bs) = do
  ys <- pairwise as bs ?:: CeCantUnifyPairwise
  forM_ ys $ \(_, (a, b)) -> unify a b
unify a b = throwError (CeCantUnify a b)

unifyMany :: [Typ] -> Check ()
unifyMany (a : b : rest) = unify a b >> unifyMany (b : rest)
unifyMany _ = pure ()

unifyLim :: DiscDiag -> DiscDiag -> Check ()
unifyLim _ _ = pure () -- TODO
