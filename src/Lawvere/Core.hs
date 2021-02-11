module Lawvere.Core where

import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.String (IsString (..))
import Lawvere.Disp
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lex

identSpecials :: [Char]
identSpecials = ['_', '\'']

pKeyword :: Parser ()
pKeyword = void $ choice $ reserved <$> Set.toList keywords

-- | Parses a reserved identifier
reserved :: Text -> Parser ()
reserved name = lexeme $
  try $ do
    _ <- chunk name
    notFollowedBy (satisfy isAlphaNum <|> oneOf identSpecials) <?> "end of " ++ toS name

keywords :: Set Text
keywords = Set.fromList ["ob", "ar", "interp", "const", "sketch", "over", "handling", "curry", "summing", "side", "if", "then", "else"]

kwOb, kwAr, kwInterp, kwConst, kwSketch, kwOver, kwHandling, kwCurry, kwSumming, kwSide, kwIf, kwThen, kwElse :: Parser ()
kwOb = reserved "ob"
kwAr = reserved "ar"
kwInterp = reserved "interp"
kwConst = reserved "const"
kwSketch = reserved "sketch"
kwOver = reserved "over"
kwHandling = reserved "handling"
kwCurry = reserved "curry"
kwSumming = reserved "summing"
kwSide = reserved "side"
kwIf = reserved "if"
kwThen = reserved "then"
kwElse = reserved "else"

identParser :: (Char -> Bool) -> Parser Text
identParser firstCond = toS <$> ((:) <$> char0 <*> charRest)
  where
    char0 = satisfy isFirst
    charRest = many (satisfy isRest)
    isFirst c = (Char.isAlpha c && firstCond c) || c `elem` identSpecials
    isRest c = Char.isAlphaNum c || c `elem` identSpecials

newtype LcIdent = LcIdent {getLcIdent :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

instance IsString LcIdent where
  fromString = LcIdent . toS

newtype UcIdent = UcIdent {getUcIdent :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

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

-- TODO: rename to something that doesn't rule out the dual.
data Label
  = LPos Int
  | LNam LcIdent
  deriving stock (Eq, Ord, Show, Generic)

instance Parsed Label where
  parsed = (LNam <$> parsed) <|> (LPos <$> Lex.decimal)

instance Disp Label where
  disp =
    \case
      LPos i -> pretty (show i :: Text)
      LNam l -> disp l

wrapped :: Char -> Char -> Parser a -> Parser a
wrapped l r = between (lexChar l) (single r)

braced :: Parser a -> Parser a
braced = wrapped '{' '}'

pWrapSep :: Char -> Char -> Char -> Parser a -> Parser [a]
pWrapSep s l r pItem = wrapped l r (sepBy1 (lexeme pItem) (lexChar s))

pCommaSep :: Char -> Char -> Parser a -> Parser [a]
pCommaSep = pWrapSep ','

pFields :: (Parsed a, Parsed k) => Char -> Char -> Char -> Maybe (k -> a) -> Parser [(k, a)]
pFields l r s punner_ = ([] <$ symbol (toS [l, s, r])) <|> pCommaSep l r pField
  where
    pField = do
      name <- lexeme parsed
      val <- case punner_ of
        Just punner -> fromMaybe (punner name) <$> optional (lexChar s *> parsed)
        Nothing -> lexChar s *> parsed
      pure (name, val)

pTuple :: Parser a -> Parser [a]
pTuple = pCommaSep '(' ')'

pBracedFields :: (Parsed a, Parsed k) => Char -> Maybe (k -> a) -> Parser [(k, a)]
pBracedFields = pFields '{' '}'

pBracketedFields :: (Parsed a, Parsed k) => Char -> Maybe (k -> a) -> Parser [(k, a)]
pBracketedFields = pFields '[' ']'

-- General util

infixr 0 ?:

-- | Convert a 'Maybe' value into an error.
(?:) :: (MonadError e m) => Maybe a -> e -> m a
x_ ?: e = case x_ of
  Nothing -> throwError e
  Just x -> pure x

infixr 0 ?::

(?::) :: (MonadError e m) => Either err a -> (err -> e) -> m a
x_ ?:: f = case x_ of
  Left e -> throwError (f e)
  Right x -> pure x

lookupRest :: (Eq k) => k -> [(k, v)] -> Maybe (v, [(k, v)])
lookupRest _ [] = Nothing
lookupRest k ((k', x) : rest)
  | k == k' = Just (x, rest)
  | otherwise = do
    (v, rest') <- lookupRest k rest
    pure (v, (k', x) : rest')

-- | Checks that keys in two associative lists match up. If not, says which key
-- is missing, from which side, and what value the other side had for that key.
pairwise :: (Eq k) => [(k, a)] -> [(k, b)] -> Either (k, Either b a) [(k, (a, b))]
pairwise [] [] = pure []
pairwise ((k, a) : as') bs = do
  (b, bs') <- lookupRest k bs ?: (k, Right a)
  rest <- pairwise as' bs'
  pure $ (k, (a, b)) : rest
pairwise [] ((k, b) : _) = Left (k, Left b)
