module Main (main, dev) where

import Control.Lens
import Control.Monad.Trans.Except
import Data.List (isSuffixOf, nub)
import qualified Data.Set as Set
import Lawvere.Check
import Lawvere.Core
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import qualified Lawvere.Instruction as Machine
import Lawvere.Literate
import Lawvere.Parse hiding (Parser)
import Options.Applicative
import Protolude hiding (empty, option)
import System.Console.Haskeline
import qualified Text.Megaparsec as Mega
import Prelude (String)

say :: (MonadIO m) => Text -> m ()
say = putStrLn

putErr :: (MonadIO m) => Text -> m ()
putErr = liftIO . hPutStrLn stderr

data Target
  = JS
  | Hask
  | VmCode
  | Vm
  deriving stock (Eq, Show)

loadFile :: (MonadIO m) => Target -> FilePath -> ExceptT Text m [Decl]
loadFile target filepath = do
  t <- liftIO (readFile filepath)
  source <-
    if ".md" `isSuffixOf` filepath
      then case litSource filepath t of
        Left err -> throwError err
        Right s -> pure s
      else pure t
  prog :: [Decl] <- except (first (toS . Mega.errorBundlePretty) (Mega.parse (parsed <* Mega.eof) filepath source))
  sayi "Checking.."
  let (res, nub -> warns) = checkProg prog
  forM_ warns (sayi . ("WARN: " <>))
  case res of
    Right _ -> sayi "Check OK!"
    Left err -> do
      putErr "😲 Oh no! A category error:"
      putErr ""
      putErr (render err)
  pure prog
  where
    sayi = sayHask target

sayHask :: (MonadIO m) => Target -> Text -> m ()
sayHask target t = when (target `elem` [Hask, Vm]) (say t)

runFile :: Target -> FilePath -> IO ()
runFile target filepath = handleErr $ do
  prog <- loadFile target filepath
  let inp = Rec mempty
  case target of
    Hask -> do
      v <- liftIO $ evalMain inp prog
      say ""
      say "output:"
      say ("  " <> render v)
    JS -> say (mkJS prog)
    VmCode -> say (render (Machine.compileProg prog))
    Vm -> do
      say "Running on categorical machine.."
      say $ "Result: " <> render (Machine.runProg (Machine.MRec mempty) prog)
  where
    handleErr m =
      runExceptT m >>= \case
        Left e -> putErr e
        Right x -> pure x

data ReplState = ReplState
  { prog :: [Decl],
    names :: Set String
  }
  deriving stock (Generic)

newtype Repl a = Repl {runRepl :: InputT (StateT ReplState IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadState ReplState Repl where
  state = Repl . lift . state

repl :: Maybe FilePath -> IO ()
repl filepath_ = do
  prog_ <- load
  case prog_ of
    Left err -> putErr err
    Right st -> do
      let compl :: (String, String) -> StateT ReplState IO (String, [Completion])
          compl = completeWord Nothing " \n()[]{}" go
            where
              go :: String -> StateT ReplState IO [Completion]
              go prefix = do
                ws <- use #names
                let ws' = Set.toList $ Set.filter (prefix `isPrefixOf`) ws
                pure (simpleCompletion <$> ws')
          setts :: Settings (StateT ReplState IO)
          setts = setComplete compl (defaultSettings {historyFile = Just ".lawvere-repl"})
      flip evalStateT st $ runInputT setts (runRepl loop)
  where
    load :: (MonadIO m) => m (Either Text ReplState)
    load = case filepath_ of
      Just filepath -> runExceptT $ do
        prog <- loadFile Hask filepath
        pure (ReplState prog (foldMap declNames prog))
      Nothing -> pure (Right (ReplState [] mempty))
    declNames :: Decl -> Set String
    declNames = \case
      DAr _ name _ _ _ -> Set.singleton (lc name)
      DOb _ name _ -> Set.singleton (uc name)
      DSketch Sketch {name, obs, ars} ->
        Set.fromList $
          [uc name]
            <> ((\SketchAr {name = n} -> lc n) <$> ars)
            <> (uc <$> obs)
      DInterp name _ _ _ _ _ -> Set.singleton (lc name)
      where
        lc (LcIdent s) = toS s
        uc (UcIdent s) = toS s
    loop :: Repl ()
    loop = do
      inp_ <- Repl (getInputLine "> ")
      case inp_ of
        Nothing -> pure ()
        Just (':' : cmd) -> case cmd of
          "q" -> Repl (outputStrLn "Bye!")
          "r" -> do
            prog'_ <- load
            case prog'_ of
              Left err -> Repl $ do
                outputStrLn "Couldn't reload:"
                outputStrLn (toS err)
              Right st -> put st >> loop
          _ -> do
            Repl $ outputStrLn ("Unrecognised command: ':" <> cmd <> "'")
            loop
        Just inp -> do
          case Mega.parse (parsed <* Mega.eof) "input" (toS inp) of
            Left err -> Repl . outputStrLn . toS . Mega.errorBundlePretty $ err
            Right expr -> do
              pr <- use #prog
              let exec = render <$> eval (Rec mempty) pr expr
              res <- liftIO $ catch exec $ \(FatalError err) -> pure ("ERROR: " <> err)
              Repl $ outputStrLn (toS res)
              loop

data Mode = Batch | Interactive

data Opts = Opts
  { mode :: Mode,
    target :: Target,
    file :: Maybe FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( (\i -> if i then Interactive else Batch)
            <$> switch
              ( long "interactive"
                  <> short 'i'
                  <> help "Interactive mode"
              )
        )
    <*> option
      (maybeReader parseTarget)
      ( long "target"
          <> help "Which compiler to target"
          <> showDefault
          <> value Hask
          <> metavar "TARGET"
      )
    <*> optional
      ( strArgument
          ( help "The source file to compile/run"
              <> metavar "FILE"
          )
      )
  where
    parseTarget (map toLower -> t) = case t of
      "js" -> Just JS
      "javascript" -> Just JS
      "hs" -> Just Hask
      "hask" -> Just Hask
      "vmcode" -> Just VmCode
      "vm" -> Just Vm
      _ -> Nothing

main :: IO ()
main = do
  Opts {..} <- execParser opts
  sayHask target $
    "--------------\n"
      <> "Lawvere v0.0.0\n"
      <> "--------------"
  case mode of
    Batch -> case file of
      Nothing -> putErr "A file must be specified when not in interactive mode."
      Just filepath -> runFile target filepath
    Interactive -> repl file
  where
    opts =
      info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Run the Lawvere compiler on FILE compiling to TARGET"
            <> header "Lawvere - A categorical programming language"
        )

dev :: IO ()
dev = do
  runFile Hask "examples/product.law"
  -- runFile Hask "examples/tutorial.law"
  -- runFile Hask "examples/io.law"
  -- runFile Hask "examples/basic.law"
  -- runFile Hask "examples/list.law"
  runFile Hask "examples/freyd-state.law"
