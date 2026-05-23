{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Wasm.Prim
import Lawvere.Check (checkProg)
import Lawvere.Core (LcIdent (..), UcIdent (..))
import Lawvere.Decl
import Lawvere.Disp (render, renderTerm)
import Lawvere.Eval (Val (..), eval)
import Lawvere.File (parseSource)
import Lawvere.Parse (parsed)
import Protolude
import qualified Text.Megaparsec as Mega
import Prelude (String)

----------------------------------------------------------------------------
-- JavaScript FFI

foreign import javascript "terminal_initialize()"
  js_initialize :: IO ()

foreign import javascript "terminal_readLine($1)"
  js_readLine :: JSString -> IO JSString

foreign import javascript unsafe "terminal_printLine($1)"
  js_printLine :: JSString -> IO ()

foreign import javascript "terminal_loadFile($1)"
  js_loadFile :: JSString -> IO JSString

foreign export javascript "hs_start" main :: IO ()

----------------------------------------------------------------------------
-- Console helpers

printLine :: Text -> IO ()
printLine = js_printLine . toJSString . toS

readLine :: Text -> IO Text
readLine prompt = toS . fromJSString <$> js_readLine (toJSString (toS prompt))

loadFileContents :: FilePath -> IO (Either Text Text)
loadFileContents fp = do
  r <- try (js_loadFile (toJSString fp))
  case r of
    Right js -> pure (Right (toS (fromJSString js)))
    Left (e :: SomeException) -> pure (Left (show e))

----------------------------------------------------------------------------
-- REPL state

data ReplState = ReplState
  { prog :: [Decl],
    names :: Set String,
    lastFile :: Maybe FilePath
  }

emptyState :: ReplState
emptyState = ReplState {prog = [], names = mempty, lastFile = Nothing}

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
  DEff _ -> mempty
  DEffInterp _ -> mempty
  where
    lc (LcIdent s) = toS s
    uc (UcIdent s) = toS s

----------------------------------------------------------------------------
-- File loading

loadProg :: FilePath -> IO (Either Text ReplState)
loadProg fp = do
  printLine ("Loading " <> toS fp <> "..")
  contentR <- loadFileContents fp
  case contentR of
    Left err -> pure (Left err)
    Right contents ->
      case runExcept (parseSource fp contents) of
        Left err -> pure (Left err)
        Right decls -> do
          printLine "Checking.."
          let (res, warns) = checkProg decls
          forM_ warns $ \w -> printLine ("WARN: " <> w)
          case res of
            Right _ -> printLine "Check OK!"
            Left err -> printLine ("Category error:\n\n" <> render err)
          pure $
            Right
              ReplState
                { prog = decls,
                  names = foldMap declNames decls,
                  lastFile = Just fp
                }

----------------------------------------------------------------------------
-- REPL loop

main :: IO ()
main = do
  js_initialize
  printLine "--------------"
  printLine "Lawvere v0.0.0 (web)"
  printLine "--------------"
  printLine "Try `40 + 2`, or `:l examples/tutorial.law`."
  printLine "Type `:l` (no arg) to open a local file. `:q` to quit."
  loop emptyState

loop :: ReplState -> IO ()
loop st = do
  raw <- readLine "> " `catch` \(_ :: SomeException) -> pure ""
  let inp = Text.strip raw
  case Text.uncons inp of
    Nothing -> loop st
    Just (':', _) -> handleCommand inp
    _ -> evalExpr inp
  where
    handleCommand cmd
      | cmd == ":q" = printLine "Bye!"
      | cmd == ":r" = case lastFile st of
          Nothing -> printLine "No file loaded; use `:l <file>` first." >> loop st
          Just fp -> reloadFrom fp
      | cmd == ":l" = reloadFrom ""
      | Just rest <- Text.stripPrefix ":l " cmd = reloadFrom (toS (Text.strip rest))
      | Just rest <- Text.stripPrefix ":load " cmd = reloadFrom (toS (Text.strip rest))
      | otherwise = printLine ("Unknown command: " <> cmd) >> loop st

    reloadFrom fp = do
      r <- loadProg fp
      case r of
        Left err -> printLine err >> loop st
        Right st' -> loop st'

    evalExpr inp =
      case Mega.parse (parsed <* Mega.eof) "<repl>" inp of
        Left err -> do
          printLine (toS (Mega.errorBundlePretty err))
          loop st
        Right expr -> do
          r <-
            ( do
                v <- eval (Rec mempty) (prog st) expr
                out <- renderTerm v
                Right <$> evaluate out
              )
              `catch` \(FatalError msg) -> pure (Left ("ERROR: " <> msg))
          case r of
            Right out -> printLine out
            Left e -> printLine e
          loop st
