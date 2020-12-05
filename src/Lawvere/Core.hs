module Lawvere.Core where

import Data.Aeson hiding ((<?>))
import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.String (IsString (..))
import Lawvere.Disp
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec

identSpecials :: [Char]
identSpecials = ['_', '\'']

keywords :: Set Text
keywords = Set.fromList ["ob", "ar", "main", "id", "const"]

pKeyword :: Parser ()
pKeyword = void $ choice $ reserved <$> Set.toList keywords

-- | Parses a reserved identifier
reserved :: Text -> Parser ()
reserved name = lexeme $
  try $ do
    _ <- chunk name
    notFollowedBy (satisfy isAlphaNum <|> oneOf identSpecials) <?> "end of " ++ toS name

kwOb, kwAr, kwMain, kwId, kwConst :: Parser ()
kwOb = reserved "ob"
kwAr = reserved "ar"
kwMain = reserved "main"
kwConst = reserved "const"
kwId = reserved "id"

identParser :: (Char -> Bool) -> Parser Text
identParser firstCond = toS <$> ((:) <$> char0 <*> charRest)
  where
    char0 = satisfy isFirst
    charRest = many (satisfy isRest)
    isFirst c = (Char.isAlpha c && firstCond c) || c `elem` identSpecials
    isRest c = Char.isAlphaNum c || c `elem` identSpecials

newtype LcIdent = LcIdent {getLcIdent :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UcIdent = UcIdent {getUcIdent :: Text}
  deriving stock (Show)
  deriving newtype (Eq, ToJSON, FromJSON)

-- FIXME: not safe
instance IsString LcIdent where
  fromString = LcIdent . toS

-- FIXME: not safe
instance IsString UcIdent where
  fromString = UcIdent . toS

instance Parsed LcIdent where
  parsed = try $ do
    i <- identParser Char.isLower
    if Set.member i keywords
      then unexpected (Label ('k' :| "eyword"))
      else pure (LcIdent i)

instance Disp LcIdent where
  disp (LcIdent x) = pretty x

instance Disp UcIdent where
  disp (UcIdent x) = pretty x

instance Parsed UcIdent where
  parsed = UcIdent <$> identParser Char.isUpper

pApp :: (a -> [a] -> a) -> Parser a -> Parser a
pApp app pAtom = do
  x <- lexeme pAtom
  fs <- many (lexeme pAtom)
  pure $ case fs of
    [] -> x
    _ -> app x fs

wrapped :: Char -> Char -> Parser a -> Parser a
wrapped l r = between (lexChar l) (single r)

pWrapSep :: Char -> Char -> Char -> Parser a -> Parser [a]
pWrapSep s l r pItem = wrapped l r (sepBy pItem (lexChar s) <* optional (single s))

pCommaSep :: Char -> Char -> Parser a -> Parser [a]
pCommaSep = pWrapSep ','

pFields :: (Parsed a) => Char -> Char -> Char -> Parser [(LcIdent, a)]
pFields l r fieldSym = pCommaSep l r pField
  where
    pField = (,) <$> lexeme parsed <*> (lexChar fieldSym *> parsed)

pTuple :: Parser a -> Parser [a]
pTuple = pCommaSep '(' ')'

pBracedFields :: (Parsed a) => Char -> Parser [(LcIdent, a)]
pBracedFields = pFields '{' '}'

pBracketedFields :: (Parsed a) => Char -> Parser [(LcIdent, a)]
pBracketedFields = pFields '[' ']'
