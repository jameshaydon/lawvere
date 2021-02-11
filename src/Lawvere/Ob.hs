module Lawvere.Ob where

import Control.Lens
import Control.Monad.Combinators.Expr
import Data.Generics.Product.Types
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many, some, try)
import Text.Megaparsec

newtype MetaVar = MkVar Int
  deriving stock (Eq, Ord, Show, Generic)

instance Disp MetaVar where
  disp (MkVar i) = "_" <> pretty i

type DiscDiag = [(Label, Ob)]

data OPrim = TInt | TFloat | TString | TBase
  deriving stock (Eq, Show, Generic)

instance Disp OPrim where
  disp p = pretty $ drop 1 (show p :: [Char])

data With
  = ObEq UcIdent Ob
  deriving stock (Eq, Show, Generic)

instance Parsed With where
  parsed = ObEq <$> lexeme parsed <*> (symbol "=" *> parsed)

instance Disp With where
  disp (ObEq a b) = disp a <+> "=" <+> disp b

data Extension = Extension UcIdent [With]
  deriving stock (Eq, Show, Generic)

instance Parsed Extension where
  parsed = wrapped '[' ']' $ do
    sketchName <- lexeme parsed
    withs <- optional (lexChar '|' *> some parsed)
    pure (Extension sketchName (concat withs))

data Ob
  = Lim DiscDiag
  | CoLim DiscDiag
  | ONamed UcIdent
  | OTuple [Ob]
  | OVar MetaVar
  | OPrim OPrim
  | Ob :=> Ob
  | TFunApp LcIdent Ob
  | OFree Ob Extension
  deriving stock (Eq, Show, Generic)

prodToLim :: [Ob] -> Ob
prodToLim as = Lim [(LPos i, f) | (i, f) <- zip [1 :: Int ..] as]

freeVars :: Traversal' Ob MetaVar
freeVars = types @MetaVar

instance Parsed Ob where
  parsed = makeExprParser pAtom operatorTable

operatorTable :: [[Operator Parser Ob]]
operatorTable = [[InfixR ((:=>) <$ symbol "=>")]]

pFunApp :: Parser Ob
pFunApp = do
  f <- try $ parsed <* lexChar '('
  x <- parsed
  _ <- single ')'
  pure (TFunApp f x)

pAtom :: Parser Ob
pAtom = lexeme $ do
  unextended <-
    choice
      [ OTuple <$> pTuple (lexeme parsed) <?> "tuple",
        Lim <$> pBracedFields ':' Nothing <?> "lim",
        CoLim <$> pBracketedFields ':' Nothing <?> "colim",
        ONamed <$> parsed <?> "named object",
        pFunApp <?> "functor application"
      ]
  extension_ <- optional (parsed <?> "extension")
  pure $ case extension_ of
    Nothing -> unextended
    Just ext -> OFree unextended ext

instance Disp Ob where
  disp = \case
    TFunApp f x -> disp f <> parens (disp x)
    Lim xs -> commaBrace ':' xs
    CoLim xs -> commaBracket ':' xs
    ONamed name -> disp name
    OVar var -> disp var
    OTuple xs -> dispTup xs
    a :=> b -> disp a <+> "=>" <+> disp b
    OPrim p -> disp p
    OFree theCat (Extension sketch withs) -> disp theCat <> brackets (go withs)
      where
        go [] = disp sketch
        go _ = disp sketch <+> "|" <+> sep (punctuate comma (disp <$> withs))
