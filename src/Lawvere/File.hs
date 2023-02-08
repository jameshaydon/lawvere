module Lawvere.File where

import Lawvere.Decl
import Lawvere.Literate
import Lawvere.Parse
import Protolude
import qualified Text.Megaparsec as Mega

parseFile :: (MonadIO m, MonadError Text m) => FilePath -> m [Decl]
parseFile filepath = do
  t <- liftIO (readFile filepath)
  source <-
    if ".md" `isSuffixOf` filepath
      then case litSource filepath t of
        Left err -> throwError err
        Right s -> pure s
      else pure t
  let parseRes = Mega.parse (parsed <* Mega.eof) filepath source
  case parseRes of
    Left err -> throwError . toS . Mega.errorBundlePretty $ err
    Right decls -> pure decls
