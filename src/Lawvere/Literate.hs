module Lawvere.Literate where

import Commonmark
import Commonmark.Pandoc
import qualified Data.Text as Text
import Protolude
import Text.Pandoc.Builder as Pandoc

litSource :: FilePath -> Text -> Either Text Text
litSource fp t =
  case commonmark fp t of
    Right (Cm blocks :: Cm () Blocks) ->
      Right $ Text.unlines $ mapMaybe lawCode (Pandoc.toList blocks)
    Left err -> Left (show err)
  where
    lawCode (CodeBlock ("", xs, _) src) | "lawvere" `elem` xs = Just src
    lawCode _ = Nothing
