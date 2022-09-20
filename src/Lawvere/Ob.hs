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

data OPrim = TInt | TFloat | TString | TBase | TLine | TTime | TMicro
  deriving stock (Eq, Ord, Show, Generic)

instance Disp OPrim where
  disp p = pretty $ drop 1 (show p :: [Char])

data Ob
  = Lim DiscDiag
  | CoLim DiscDiag
  | ONamed UcIdent
  | OTuple [Ob]
  | OVar MetaVar
  | OPrim OPrim
  | Ob :=> Ob
  | TFunApp LcIdent Ob
  | OFree Ob [UcIdent]
  | SumOb LcIdent
  deriving stock (Eq, Show, Generic)

instance Parsed Ob where
  parsed = makeExprParser pAtom operatorTable

instance Plated Ob where
  plate f (Lim diag) = Lim <$> traverse (_2 f) diag
  plate f (CoLim diag) = CoLim <$> traverse (_2 f) diag
  plate _ named@(ONamed _) = pure named
  plate f (OTuple as) = OTuple <$> traverse f as
  plate _ (OVar v) = pure (OVar v)
  plate _ (OPrim p) = pure (OPrim p)
  plate f (a :=> b) = (:=>) <$> f a <*> f b
  plate f (TFunApp fun o) = TFunApp fun <$> f o
  plate f (OFree o extension) = OFree <$> f o <*> pure extension
  plate _ so@(SumOb _) = pure so

prodToLim :: [Ob] -> Ob
prodToLim as = Lim [(LPos i, f) | (i, f) <- zip [1 :: Int ..] as]

freeVars :: Traversal' Ob MetaVar
freeVars = types @MetaVar

data Niche a = Niche a a
  deriving stock (Show)

instance Parsed a => Parsed (Niche a) where
  parsed = Niche <$> (lexeme parsed <* symbol "-->") <*> parsed

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
      [ SumOb <$> pBuiltin1 "SumOb",
        OTuple <$> pTuple (lexeme parsed) <?> "tuple",
        Lim <$> pBracedFields ':' Nothing <?> "lim",
        CoLim <$> pBracketedFields ':' Nothing <?> "colim",
        ONamed <$> parsed <?> "named object",
        pFunApp <?> "functor application"
      ]
  extension_ <- optional (pCommaSep '[' ']' parsed)
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
    OFree theCat effs -> disp theCat <> brackets (hsep (punctuate comma (disp <$> effs)))
    SumOb idx -> "SubOb" <> parens (disp idx)
