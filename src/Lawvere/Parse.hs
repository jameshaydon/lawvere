module Lawvere.Parse where

import Protolude hiding (many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO: use this solution for collecting/displaying source errors:
-- https://github.com/mrkkrp/megaparsec/issues/343

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

-- Parse a thing and then trailing whitespace after that thing.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses a character as a token
lexChar :: Char -> Parser ()
lexChar = void . lexeme . single

-- Parse a chunk of text and all trailing whitespace after it.
symbol :: Text -> Parser ()
symbol = void . L.symbol sc

class Parsed a where
  parsed :: Parser a

some' :: MonadPlus m => m a -> m (a, [a])
some' p = liftM2 (,) p (many p)
{-# INLINE some' #-}
