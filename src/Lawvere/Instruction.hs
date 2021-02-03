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
  | Addr Int
  deriving stock (Eq, Ord, Show)

instance Disp Addr where
  disp (Addr i) = "#" <> pretty i
  disp (AddrTop label) = "#" <> disp label

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
  | IPlus
  | IApp
  deriving stock (Show)

prims :: Code
prims =
  Map.fromList
    [ (AddrTop (LcIdent "plus"), [IPlus]),
      (AddrTop (LcIdent "app"), [IApp])
    ]

type Code = Map Addr [Instr]

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

type Comp a = State CompilerState a

storeCode :: [Instr] -> Comp Addr
storeCode code = do
  a <- gets nextAddr
  #nextAddr .= a + 1
  #code %= Map.insert (Addr a) code
  pure (Addr a)

noEffects :: [(ConeComponent, Expr)] -> Comp [(Label, Expr)]
noEffects = panic "TODO"

compile :: Expr -> Comp [Instr]
compile = \case
  EFunApp _ _ -> panic "TODO"
  EPrim _ -> panic "TODO"
  ELim _ -> panic "TODO"
  ECoLim _ -> panic "TODO"
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
  _ -> panic "can't compile to vm code"
  where
    compStore = compile >=> storeCode

compileDecl :: Decl -> Comp ()
compileDecl (DAr _ name _ _ e) = do
  code <- compile e
  #code %= Map.insert (AddrTop name) code
compileDecl DOb {} = pure ()
compileDecl DSketch {} = pure ()

compileProg :: Decls -> Code
compileProg ds = prims <> view #code (execState (traverse_ compileDecl ds) initCompilerState)

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
  IPlus -> do
    x <- use #current
    case x of
      MRec r
        | Just (MSca (Int a)) <- lkp (LPos 1) r,
          Just (MSca (Int b)) <- lkp (LPos 2) r ->
          #current .= MSca (Int (a + b))
      _ -> panic "bad plus"
  IApp -> do
    x <- use #current
    case x of
      MRec r
        | Just (MFun ff) <- lkp (LPos 1) r,
          Just aa <- lkp (LPos 2) r -> do
          #current .= aa
          call ff
      v -> panic ("bad app: " <> show v)
  where
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

runProg :: Val -> Decls -> Val
runProg v ds =
  let code = compileProg ds
      initMach =
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
   in execState (steps 2000) initMach ^. #current
