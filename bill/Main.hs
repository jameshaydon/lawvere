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

loadFile :: (MonadIO m) => Bool -> Target -> FilePath -> ExceptT Text m [Decl]
loadFile warnings target filepath = do
  sayi . toS $ "Loading " <> filepath <> ".."
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
  when warnings $ forM_ warns (sayi . ("WARN: " <>))
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

runFile :: Bool -> Target -> FilePath -> IO ()
runFile warnings target filepath = handleErr $ do
  prog <- loadFile warnings target filepath
  let inp = Rec mempty
  case target of
    Hask -> do
      v <- liftIO $ evalMain inp prog
      say ""
      liftIO (renderTerm v) >>= say
    JS -> liftIO (mkJS prog) >>= say
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

repl :: Bool -> Maybe FilePath -> IO ()
repl warnings filepath_ = do
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
        prog <- loadFile warnings Hask filepath
        pure (ReplState prog (foldMap declNames prog))
      Nothing -> pure (Right (ReplState [] mempty))
    declNames :: Decl -> Set String
    declNames = \case
      DAr _ name _ _ -> Set.singleton (lc name)
      DOb _ name _ -> Set.singleton (uc name)
      DSketch Sketch {name, obs, ars} ->
        Set.fromList $
          [uc name]
            <> ((\SketchAr {name = n} -> lc n) <$> ars)
            <> (uc <$> obs)
      DInterp name _ _ _ _ _ -> Set.singleton (lc name)
      DCategory Category {catName} -> Set.singleton (uc catName)
      DEffCat EffCat {effCatStructName} -> Set.singleton (lc effCatStructName)
      DEff _ -> mempty -- TODO
      DEffInterp _ -> mempty
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
            Left err -> do
              Repl . outputStrLn . toS . Mega.errorBundlePretty $ err
              loop
            Right expr -> do
              pr <- use #prog
              let exec = Right <$> eval (Rec mempty) pr expr
              res <- liftIO $ catch exec $ \(FatalError err) -> pure (Left ("ERROR: " <> err))
              case res of
                Right v -> do
                  out <- liftIO (renderTerm v)
                  Repl $ outputStrLn (toS out)
                Left err -> Repl $ outputStrLn (toS err)
              loop

data Mode = Batch | Interactive

data Opts = Opts
  { mode :: Mode,
    target :: Target,
    file :: Maybe FilePath,
    warnings :: Bool
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
      ( short 't'
          <> long "target"
          <> help "Which compiler ('hs', 'js', 'vmcode' or 'vm') to target, defaults to 'hs'"
          <> value Hask
          <> metavar "TARGET"
      )
    <*> optional
      ( strArgument
          ( help "The source file to compile/run"
              <> metavar "FILE"
          )
      )
    <*> ( not
            <$> switch
              ( long "no-warnings"
                  <> short 'w'
                  <> help "Turn off warnings"
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
      Just filepath -> runFile warnings target filepath
    Interactive -> repl warnings file
  where
    opts =
      info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Run the Lawvere compiler on FILE compiling to TARGET"
            <> header "Lawvere - A categorical programming language"
        )

dev' :: IO ()
dev' = do
  runFile False Hask "examples/product.law"
  runFile False Hask "examples/tutorial.law"
  runFile False Hask "examples/io.law"
  runFile False Hask "examples/basic.law"
  runFile False Hask "examples/list.law"
  -- runFile Hask "examples/freyd-state.law"
  runFile False Hask "examples/partial-state.law"

dev :: IO ()
dev = dev' `catch` (\(FatalError err) -> putStrLn err)
