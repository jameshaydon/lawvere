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

testExample :: Text -> IO ()
testExample name = do
  let fileName = name <> ".law"
  say ""
  say "--------------------"
  say $ "Testing: " <> fileName
  say "--------------------"
  say ""
  example <- readFile (toS ("examples/" <> fileName))
  case Mega.parse (parsed <* Mega.eof) (toS fileName) example of
    Left err -> putStr (Mega.errorBundlePretty err)
    Right (prog :: [Decl]) -> do
      --putStrLn (render prog)
      case checkProg prog of
        Right _ -> say "Check OK!"
        Left err -> do
          say "Check ERROR!"
          say (render err)
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
      --say "virtual machine:"
      --let res = Machine.runProg (Machine.MRec mempty) prog
      --say ("  " <> render res)
      pure ()

main :: IO ()
main = do
  testExample "basic"
  testExample "list"
  testExample "error"
  testExample "state"

--parseTest @Ob parsed "Base[State]"

say :: Text -> IO ()
say = putStrLn
