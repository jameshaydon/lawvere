module Main where

import Lawvere.Check
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
--import qualified Lawvere.Instruction as Machine
--import Lawvere.Ob
import Lawvere.Parse
import Protolude hiding (empty)
import qualified Text.Megaparsec as Mega

parseTest :: Parser a -> Text -> IO ()
parseTest p inp =
  case Mega.parse (p <* Mega.eof) "parse test" inp of
    Left err -> putStr (Mega.errorBundlePretty err)
    Right _ -> pure ()

say :: Text -> IO ()
say = putStrLn

runFile :: FilePath -> IO ()
runFile filepath = do
  say "Lawvere v0.0.0"
  say "-------"
  source <- readFile filepath
  case Mega.parse (parsed <* Mega.eof) filepath source of
    Left err -> say . toS $ Mega.errorBundlePretty err
    Right (prog :: [Decl]) -> do
      say "checking.."
      case checkProg prog of
        Right _ -> say "Check OK!"
        Left err -> do
          say "Oh no, a category error:"
          say (render err)
      let inp = Rec mempty
      say ""
      say "input:"
      say ("  " <> render inp)
      -- Haskell:
      say "output:"
      v <- eval inp prog
      putStrLn ("  " <> render v)
      -- Javascript:
      --putStrLn (mkJS prog)
      -- Machine:
      --say "virtual machine:"
      --let res = Machine.runProg (Machine.MRec mempty) prog
      --say ("  " <> render res)
      pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    _ -> say "Please specify exactly one file."
