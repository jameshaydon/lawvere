module Main (main, dev) where

import Control.Monad.Trans.Except
import Data.List (isSuffixOf, nub)
import Lawvere.Check
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import Lawvere.Literate
import Lawvere.Parse
import Protolude hiding (empty)
import qualified Text.Megaparsec as Mega

say :: (MonadIO m) => Text -> m ()
say = putStrLn

putErr :: (MonadIO m) => Text -> m ()
putErr = liftIO . hPutStrLn stderr

data Target
  = JS
  | Hask
  deriving stock (Eq)

runFile :: Target -> FilePath -> IO ()
runFile target filepath = handleErr $ do
  sayi "--------------"
  sayi "Lawvere v0.0.0"
  sayi "--------------"
  t <- liftIO (readFile filepath)
  source <-
    if ".md" `isSuffixOf` filepath
      then case litSource filepath t of
        Left err -> throwError err
        Right s -> pure s
      else pure t
  prog :: [Decl] <- except (first (toS . Mega.errorBundlePretty) (Mega.parse (parsed <* Mega.eof) filepath source)) -- toS $ Mega.errorBundlePretty err
  sayi "Checking.."
  let (res, nub -> warns) = checkProg prog
  forM_ warns (sayi . ("WARN: " <>))
  case res of
    Right _ -> sayi "Check OK!"
    Left err -> do
      putErr "😲 Oh no! A category error:"
      putErr ""
      putErr (render err)
  let inp = Rec mempty
  sayi ""
  case target of
    Hask -> do
      say "input:"
      say ("  " <> render inp)
      say ""
      v <- liftIO $ eval inp prog
      say ""
      say "output:"
      say ("  " <> render v)
    JS -> putStrLn (mkJS prog)
  where
    sayi t = when (target == Hask) (say t)
    handleErr m =
      runExceptT m >>= \case
        Left e -> putErr e
        Right x -> pure x

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile Hask filename
    ["--js", filename] -> runFile JS filename
    _ -> say "Please specify exactly one file, and optionally the --js option."

dev :: IO ()
dev = do
  runFile Hask "examples/product.law"
  -- runFile Hask "examples/tutorial.law"
  -- runFile Hask "examples/io.law"
  -- runFile Hask "examples/basic.law"
  -- runFile Hask "examples/list.law"
  runFile Hask "examples/freyd-state.law"
