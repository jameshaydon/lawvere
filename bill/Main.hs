module Main (main, dev) where

import Control.Monad.Trans.Except
import Data.List (isSuffixOf, nub)
import Lawvere.Check
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import Lawvere.Literate
import Lawvere.Parse hiding (Parser)
import Options.Applicative
import Protolude hiding (empty, option)
import System.Console.Haskeline
import qualified Text.Megaparsec as Mega

say :: (MonadIO m) => Text -> m ()
say = putStrLn

putErr :: (MonadIO m) => Text -> m ()
putErr = liftIO . hPutStrLn stderr

data Target
  = JS
  | Hask
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
sayHask target t = when (target == Hask) (say t)

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
    JS -> putStrLn (mkJS prog)
  where
    handleErr m =
      runExceptT m >>= \case
        Left e -> putErr e
        Right x -> pure x

repl :: Maybe FilePath -> IO ()
repl filepath_ = do
  prog_ <- load
  case prog_ of
    Left err -> putErr err
    Right prog -> runInputT defaultSettings (loop prog)
  where
    load :: (MonadIO m) => m (Either Text [Decl])
    load = case filepath_ of
      Just filepath -> runExceptT (loadFile Hask filepath)
      Nothing -> pure (Right [])
    loop :: [Decl] -> InputT IO ()
    loop prog = do
      inp_ <- getInputLine "> "
      case inp_ of
        Nothing -> pure ()
        Just (':' : cmd) -> case cmd of
          "q" -> outputStrLn "Bye!"
          "r" -> do
            prog'_ <- load
            case prog'_ of
              Left err -> do
                outputStrLn "Couldn't reload:"
                outputStrLn (toS err)
              Right prog' -> loop prog'
          _ -> do outputStrLn ("Unrecognised command: ':" <> cmd <> "'")
        Just inp -> do
          case Mega.parse (parsed <* Mega.eof) "input" (toS inp) of
            Left err -> outputStrLn (toS (Mega.errorBundlePretty err))
            Right expr -> do
              let exec = render <$> eval (Rec mempty) prog expr
              res <- liftIO $ catch exec $ \(FatalError err) -> pure ("ERROR: " <> err)
              outputStrLn (toS res)
              loop prog

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
