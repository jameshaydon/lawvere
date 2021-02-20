module Lawvere.Disp where

import Prettyprinter
import qualified Prettyprinter.Render.Terminal as RTerm
import Prettyprinter.Render.Text as Text
import Protolude
import qualified System.Console.ANSI as Term
import qualified System.Console.Terminal.Size as Term

data Ann = AnStr | AnNum | AnCons

class Disp a where
  disp :: a -> Doc Ann

renderSimple :: (Disp a) => a -> Text
renderSimple = renderStrict . layoutPretty (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}) . disp

render :: (Disp a) => a -> Text
render x =
  renderStrict $
    layoutSmart (LayoutOptions {layoutPageWidth = AvailablePerLine 62 1.0}) (disp x)

renderTerm :: Disp a => a -> IO Text
renderTerm x = do
  hasAnsi <- Term.hSupportsANSI stdout
  w_ <- Term.size
  let wid = maybe 80 Term.width w_
      opts = LayoutOptions {layoutPageWidth = AvailablePerLine wid 1.0}
      docStream = layoutSmart opts (disp x)
      rdr = if hasAnsi then RTerm.renderStrict else Text.renderStrict
  pure (rdr (reAnnotateS style docStream))

style :: Ann -> RTerm.AnsiStyle
style = \case
  AnStr -> RTerm.color RTerm.Green
  AnNum -> RTerm.colorDull RTerm.Yellow
  AnCons -> RTerm.color RTerm.Magenta <> RTerm.italicized

wrapSep :: Char -> Char -> Char -> [Doc Ann] -> Doc Ann
wrapSep s l r items =
  pretty l <+> align (sep (go items)) <+> pretty r
  where
    go (x : rest@(_ : _)) = (x <> pretty s) : go rest
    go xs = xs

commaSep :: Char -> Char -> [Doc Ann] -> Doc Ann
commaSep = wrapSep ','

commaFields :: (Disp k, Disp v) => Char -> Char -> Char -> [(k, v)] -> Doc Ann
commaFields l r s [] = pretty [l, s, r]
commaFields l r s fields = commaSep l r (dispField <$> fields)
  where
    dispField (k, v) = hang 2 $ sep [disp k <+> pretty s, disp v]

commaBracket :: (Disp k, Disp v) => Char -> [(k, v)] -> Doc Ann
commaBracket = commaFields '[' ']'

commaBrace :: (Disp k, Disp v) => Char -> [(k, v)] -> Doc Ann
commaBrace = commaFields '{' '}'

dispTup :: (Disp a) => [a] -> Doc Ann
dispTup xs = commaSep '(' ')' (disp <$> xs)
