module LawvereSpec where

import Lawvere.Check
import Lawvere.Disp
import Lawvere.Eval
import Lawvere.File
import Paths_lawvere
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "Examples work" $ do
  mainWorksSpec
    "README.md"
    "(suc.\n\
    \  { state = 3,\n\
    \    value =\n\
    \      (cons.\n\
    \        { head = 0,\n\
    \          tail =\n\
    \            (cons.\n\
    \              { head = 1,\n\
    \                tail = (cons. { head = 2, tail = (empty. {=}) }) }) }) })"
  "bool" ~> "(true. {=})"
  "sum" ~> "3"
  "interpolation" ~> "\"James likes playing Go.\""
  "basic" ~> "(cons. { head = 4, tail = (cons. { head = 3, tail = (empty. {=}) }) })"
  "hello" ~> "45"
  "list-simple" ~> "60"
  "points" ~> "\"Player B is winning by 10 points!\""
  "product" ~> "2.3"
  "positional-product" ~> "2.3"
  "tutorial"
    ~> "(cons.\n\
       \  { head = { x = 100.0, y = 100.0 },\n\
       \    tail =\n\
       \      (cons.\n\
       \        { head = { x = 1.2, y = 1.2 },\n\
       \          tail =\n\
       \            (cons. { head = { x = 0.0, y = 0.0 }, tail = (empty. {=}) }) }) })"
  "list"
    ~> "(cons.\n\
       \  { head = 2,\n\
       \    tail =\n\
       \      (cons.\n\
       \        { head = 3,\n\
       \          tail =\n\
       \            (cons.\n\
       \              { head = 0,\n\
       \                tail = (cons. { head = 3, tail = (empty. {=}) }) }) }) })"
  "partial-state"
    ~> "(suc.\n\
       \  { state = 2,\n\
       \    value =\n\
       \      (cons. { head = 0, tail = (cons. { head = 1, tail = (empty. {=}) }) }) })"
  "list-sketch" ~> "6"
  "list-trace" ~> "3"
  where
    fp ~> res = mainWorksSpec ("examples/" <> fp <> ".law") res

runMain :: (MonadIO m, MonadError Text m) => FilePath -> m Text
runMain filepath = do
  fp <- liftIO $ getDataFileName filepath
  prog <- parseFile fp
  let (res, _) = checkProg prog
  case res of
    Right _ -> do
      v <- liftIO $ evalMain (Rec mempty) prog
      pure (renderSimple v)
    Left err -> throwError (render err)

mainWorksSpec :: FilePath -> Text -> Spec
mainWorksSpec filepath expect =
  it ("can run: " <> filepath) $
    example $ do
      res <- runExceptT $ runMain filepath
      case res of
        Left err -> expectationFailure (toS err)
        Right v -> v `shouldBe` expect
