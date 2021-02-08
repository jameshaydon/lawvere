module Main (main, dev) where

import Lawvere.Check
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import Lawvere.Parse
import Protolude hiding (empty)
import qualified Text.Megaparsec as Mega

say :: Text -> IO ()
say = putStrLn

data Target
  = JS
  | Hask
  deriving stock (Eq)

runFile :: Target -> FilePath -> IO ()
runFile target filepath = do
  sayi "--------------"
  sayi "Lawvere v0.0.0"
  sayi "--------------"
  source <- readFile filepath
  case Mega.parse (parsed <* Mega.eof) filepath source of
    Left err -> say . toS $ Mega.errorBundlePretty err
    Right (prog :: [Decl]) -> do
      sayi "Checking.."
      let (res, warns) = checkProg prog
      forM_ warns (putErr . ("WARN: " <>))
      case res of
        Right _ -> sayi "Check OK!"
        Left err -> do
          putErr "😲 Oh no! A category error:"
          putErr ""
          putErr (render err)
      let inp = Rec mempty
      say ""
      case target of
        Hask -> do
          say "input:"
          say ("  " <> render inp)
          say ""
          v <- eval inp prog
          say ""
          say "output:"
          say ("  " <> render v)
        JS -> putStrLn (mkJS prog)
  where
    sayi t = when (target == Hask) (say t)
    putErr = hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile Hask filename
    ["--js", filename] -> runFile JS filename
    _ -> say "Please specify exactly one file, and optionally the --js option."

dev :: IO ()
dev = do
  --runFile "examples/product.law"
  --runFile Hask "examples/tutorial.law"
  runFile Hask "examples/io.law"

-- runFile "examples/basic.law"
--runFile Hask "examples/list.law"
--runFile Hask "examples/freyd-state.law"
