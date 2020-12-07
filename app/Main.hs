module Main where

import Lawvere.Check
import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import qualified Lawvere.Instruction as Machine
import Lawvere.Parse
import Protolude hiding (empty)
import qualified Text.Megaparsec as Mega

main :: IO ()
main = do
  basic <- readFile "examples/basic.law"
  case Mega.parse (parsed <* Mega.eof) "basic.law" basic of
    Left err -> putStr (Mega.errorBundlePretty err)
    Right (prog :: [Decl]) -> do
      putStrLn (render prog)
      say (show (checkProg prog))
      let inp = Rec mempty
      v <- eval inp prog
      say "---------------------------"
      say "input:"
      say ("  " <> render inp)
      -- Haskell:
      say "haskell interpreter:"
      putStrLn ("  " <> render v)
      -- Javascript:
      --putStrLn (mkJS prog)
      -- Machine:
      say "virtual machine:"
      let res = Machine.runProg (Machine.MRec mempty) prog
      say ("  " <> render res)

say :: Text -> IO ()
say = putStrLn
