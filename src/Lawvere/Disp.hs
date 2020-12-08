module Lawvere.Disp where

import Prettyprinter
import Prettyprinter.Render.Text
import Protolude

data Ann = Ann

class Disp a where
  disp :: a -> Doc Ann

render :: (Disp a) => a -> Text
render x =
  renderStrict $
    layoutSmart (LayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}) (disp x)

wrapSep :: Char -> Char -> Char -> [Doc Ann] -> Doc Ann
wrapSep _ l r [] = pretty [l, r]
wrapSep s l r items = pretty l <+> align (sep (go items)) <+> pretty r
  where
    go (x : rest@(_ : _)) = (x <> pretty s) : go rest
    go xs = xs

commaSep :: Char -> Char -> [Doc Ann] -> Doc Ann
commaSep = wrapSep ','

commaFields :: (Disp k, Disp v) => Char -> Char -> Char -> [(k, v)] -> Doc Ann
commaFields l r fieldSym fields = commaSep l r (dispField <$> fields)
  where
    dispField (k, v) = disp k <+> pretty fieldSym <+> disp v

commaBracket :: (Disp k, Disp v) => Char -> [(k, v)] -> Doc Ann
commaBracket = commaFields '[' ']'

commaBrace :: (Disp k, Disp v) => Char -> [(k, v)] -> Doc Ann
commaBrace = commaFields '{' '}'

dispTup :: (Disp a) => [a] -> Doc Ann
dispTup xs = commaSep '(' ')' (disp <$> xs)
