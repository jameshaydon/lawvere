module Lawvere.Instruction where

import Control.Lens
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Scalar
import Prettyprinter
import Protolude
import Prelude (lookup)

data Addr
  = AddrTop LcIdent
  | AddrSub LcIdent Int
  deriving stock (Eq, Ord, Show)

instance Disp Addr where
  disp (AddrTop label) = "#" <> disp label
  disp (AddrSub label i) = "#" <> disp label <> "_" <> pretty i

data Instr
  = IConeFinish [Label]
  | ICoCone [(Label, Addr)]
  | IPushCurrentToValStack
  | ISca Sca
  | IProj Label
  | IInj Label
  | IDistr Label
  | ICall Addr
  | IPutFun Addr
  | IPushCurrentToConeStack
  | IPopValStackToCurrent
  | IPrim Prim
  deriving stock (Show)

instance Disp Instr where
  disp =
    \case
      IPrim p -> "prim" <+> disp p
      ICall addr -> "call" <+> disp addr
      ISca s -> "scal" <+> disp s
      IDistr lab -> "distr" <+> disp lab
      ICoCone cases -> "cocone" <+> hsep [disp lab <> ":" <> disp addr | (lab, addr) <- cases]
      IPushCurrentToValStack -> "push"
      IPushCurrentToConeStack -> "push_cone"
      IPopValStackToCurrent -> "pop"
      IConeFinish labs -> "end_cone" <+> hsep (disp <$> labs)
      IInj lab -> disp lab <> "."
      IProj lab -> "." <> disp lab
      IPutFun addr -> "put" <+> disp addr

type Code = Map Addr [Instr]

instance Disp [Instr] where
  disp is = vsep (disp <$> is)

instance Disp Code where
  disp codes =
    vsep
      [ vsep
          [ disp addr <> colon,
            indent 2 (disp code)
          ]
        | (addr, code) <- Map.toList codes
      ]

data CompilerState = CompilerState
  { nextAddr :: Int,
    code :: Code
  }
  deriving stock (Generic)

initCompilerState :: CompilerState
initCompilerState =
  CompilerState
    { nextAddr = 0,
      code = mempty
    }

type Comp a = ReaderT LcIdent (State CompilerState) a

storeCode :: [Instr] -> Comp Addr
storeCode code = do
  top <- ask
  a <- gets nextAddr
  #nextAddr .= a + 1
  #code %= Map.insert (AddrSub top a) code
  pure (AddrSub top a)

noEffects :: [(ConeComponent, Expr)] -> Comp [(Label, Expr)]
noEffects = traverse go
  where
    go (ConeComponent Pure lab, e) = pure (lab, e)
    go _ = panic "can't compile to VM effects yet"

compile :: Expr -> Comp [Instr]
compile = \case
  EFunApp _ _ -> panic "TODO 2"
  EPrim prim -> pure [IPrim prim]
  ELim _ -> panic "TODO 4"
  ECoLim _ -> panic "TODO 5"
  Cone ps -> do
    ps' <- noEffects ps
    codes <- traverse (_2 compile) ps'
    pure $ [i | (_, code) <- codes, i <- eachProj code] ++ [IConeFinish (fst <$> codes)]
    where
      eachProj code =
        [IPushCurrentToValStack]
          ++ code
          ++ [ IPushCurrentToConeStack,
               IPopValStackToCurrent
             ]
  CoCone is -> pure . ICoCone <$> traverse (_2 compStore) is
  Tuple xs -> compile (tupleToCone xs)
  Lit x -> pure [ISca x]
  Proj p -> pure [IProj p]
  Inj i -> pure [IInj i]
  Distr p -> pure [IDistr p]
  Top t -> pure [ICall (AddrTop t)]
  EConst e -> do
    a <- compStore e
    pure [IPutFun a]
  Comp es -> concat <$> traverse compile es
  BinOp o f g -> compile (Comp [Cone [(ConeComponent Pure (LPos 1), f), (ConeComponent Pure (LPos 2), g)], EPrim (PrimOp o)])
  e -> panic $ "can't compile to vm code: " <> render e
  where
    compStore = compile >=> storeCode

compileDecl :: Decl -> Comp ()
compileDecl (DAr _ name _ _ e) = local (const name) $ do
  code <- compile e
  #code %= Map.insert (AddrTop name) code
compileDecl DOb {} = pure ()
compileDecl DSketch {} = pure ()
compileDecl _ = panic "TODO 6"

compileProg :: Decls -> Code
compileProg ds = view #code $ flip execState initCompilerState $ flip runReaderT (LcIdent "main") (traverse_ compileDecl ds)

-- * Machine

data Val
  = MRec (Map Label Val)
  | MTag Label Val
  | MSca Sca
  | MFun Addr
  deriving stock (Show)

instance Disp Val where
  disp = \case
    MSca s -> disp s
    MRec r -> commaBrace '=' (Map.toList r)
    MTag t v -> disp t <> "." <> disp v
    MFun f -> "FUN" <> disp f

data MachState = MachState
  { idx :: (Addr, Int),
    jump :: Maybe (Addr, Int),
    callStack :: [(Addr, Int)],
    current :: Val,
    valueStack :: [Val],
    coneStack :: [Val],
    code :: Code,
    finished :: Bool
  }
  deriving stock (Generic)

type Mach a = State MachState a

exec :: Instr -> Mach ()
exec instr = case instr of
  IConeFinish labels -> do
    xs <- replicateM (length labels) popConeStack
    #current .= MRec (Map.fromList (zip labels (reverse xs)))
  ICoCone codes -> do
    x <- use #current
    case x of
      MTag t y -> case lookup t codes of
        Just addr -> do
          #current .= y
          call addr
        Nothing -> panic "cocone got unexpected tag"
      _ -> panic "cocone got a non tag"
  IPushCurrentToValStack -> do
    x <- use #current
    #valueStack %= (x :)
  ISca x -> #current .= MSca x
  IProj p -> do
    x <- use #current
    case x of
      MRec r -> case Map.lookup p r of
        Just y -> #current .= y
        Nothing -> panic "proj got record with missing component"
      _ -> panic "proj got non record"
  IInj i -> #current %= MTag i
  IDistr label -> do
    x <- use #current
    case x of
      MRec r -> case Map.lookup label r of
        Just y -> case y of
          MTag t z -> #current .= MTag t (MRec (Map.insert label z r))
          _ -> panic "distr found at label a non-tag"
        Nothing -> panic "distr got record with missing component"
      _ -> panic "distr got non record"
  ICall a -> call a
  IPutFun a -> #current .= MFun a
  IPushCurrentToConeStack -> do
    x <- use #current
    #coneStack %= (x :)
  IPopValStackToCurrent -> do
    x <- popSomeStack "value" #valueStack
    #current .= x
  IPrim p ->
    case p of
      PrimIdentity -> pure ()
      PrimApp -> do
        x <- use #current
        case x of
          MRec r
            | Just (MFun ff) <- lkp (LPos 1) r,
              Just aa <- lkp (LPos 2) r -> do
              #current .= aa
              call ff
          v -> panic ("bad app: " <> show v)
      PrimIncr -> do
        x <- use #current
        case x of
          MSca (Int x') -> #current .= MSca (Int (x' + 1))
          _ -> panic "bad incr"
      PrimAbs -> do
        x <- use #current
        case x of
          MSca (Int x') -> #current .= MSca (Int (abs x'))
          MSca (Float x') -> #current .= MSca (Float (abs x'))
          _ -> panic "bad abs"
      PrimShow -> #current %= MSca . Str . render
      PrimOp o -> do
        x <- use #current
        case x of
          MRec r
            | Just (MSca a) <- lkp (LPos 1) r,
              Just (MSca b) <- lkp (LPos 2) r ->
              #current .= binOp MSca boolToVal o a b
          _ -> panic "bad binop"
  where
    boolToVal True = MTag (LNam "true") (MRec mempty)
    boolToVal False = MTag (LNam "false") (MRec mempty)

    call a = do
      pushToCallStack
      #jump .= Just (a, 0)

    pushToCallStack = do
      (name, i) <- use #idx
      #callStack %= ((name, i + 1) :)

    popSomeStack :: Text -> Lens' MachState [a] -> Mach a
    popSomeStack desc l = do
      xs <- use l
      case xs of
        x : rest -> do
          _ <- l .= rest
          pure x
        _ -> panic $ desc <> " stack was empty"

    popConeStack :: Mach Val
    popConeStack = popSomeStack "cone" #coneStack

    lkp = Map.lookup

step :: Mach ()
step = do
  jump_ <- use #jump
  case jump_ of
    Nothing -> do
      (a, i) <- use #idx
      codes <- use #code
      case Map.lookup a codes of
        Just instructions ->
          case drop i instructions of
            [] -> do
              goBacks <- use #callStack
              case goBacks of
                [] -> #finished .= True
                toHere : rest -> do
                  #callStack .= rest
                  #idx .= toHere
            (instr : _) -> do
              exec instr
              #idx .= (a, i + 1)
        Nothing -> panic $ "couldnt locate code address: " <> show a
    Just toHere -> do
      #jump .= Nothing
      #idx .= toHere

steps :: Int -> Mach ()
steps maxSteps | maxSteps <= 0 = pure ()
steps maxSteps = do
  step
  weAreDone <- use #finished
  if weAreDone
    then pure ()
    else steps (maxSteps - 1)

-- * Run program

initMach :: Val -> Code -> MachState
initMach v code =
  MachState
    { idx = (AddrTop (LcIdent "main"), 0),
      jump = Nothing,
      callStack = [],
      current = v,
      valueStack = [],
      coneStack = [],
      finished = False,
      code
    }

runCode :: Val -> Code -> Val
runCode v code = execState (steps 4000) (initMach v code) ^. #current

runProg :: Val -> Decls -> Val
runProg v ds =
  let code = compileProg ds
   in runCode v code
