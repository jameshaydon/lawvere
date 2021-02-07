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

runFile :: FilePath -> IO ()
runFile filepath = do
  say "--------------"
  say "Lawvere v0.0.0"
  say "--------------"
  source <- readFile filepath
  case Mega.parse (parsed <* Mega.eof) filepath source of
    Left err -> say . toS $ Mega.errorBundlePretty err
    Right (prog :: [Decl]) -> do
      say "Checking.."
      case checkProg prog of
        Right _ -> say "Check OK!"
        Left err -> do
          say "😲 Oh no! A category error:"
          say ""
          say (render err)
      let inp = Rec mempty
      say ""
      say "input:"
      say ("  " <> render inp)
      say "output:"
      v <- eval inp prog
      say ("  " <> render v)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    _ -> say "Please specify exactly one file."

dev :: IO ()
dev = do
  --runFile "examples/product.law"
  runFile "examples/tutorial.law"
  -- runFile "examples/basic.law"
  runFile "examples/list.law"
  runFile "examples/freyd-state.law"
