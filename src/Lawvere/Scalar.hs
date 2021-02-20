module Lawvere.Scalar where

import Lawvere.Disp
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (try)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

data Sca
  = Int Integer
  | Float Double
  | Str Text
  deriving stock (Eq, Show, Generic)

instance Disp Sca where
  disp = \case
    Int i -> annotate AnNum (pretty i)
    Float d -> annotate AnNum (pretty d)
    -- Prints escape sequences just like Haskell.
    Str s -> annotate AnStr (pretty (show s :: Text))

pNum :: Parser Sca
pNum = do
  n <- try (Right <$> L.float) <|> (Left <$> L.decimal)
  pure $ case n of
    Left i -> Int i
    Right d -> Float d

-- Parsing of string literals handles escape sequences just like Haskell.
stringP :: Parser Text
stringP = toS <$> escapedString
  where
    escapedString = catMaybes <$> (Char.char '"' >> manyTill ch (Char.char '"'))
    ch = (Just <$> L.charLiteral) <|> (Nothing <$ Char.string "\\&")

instance Parsed Sca where
  parsed =
    choice
      [ pNum,
        Str <$> stringP
      ]
