module Lawvere.Typ where

import Control.Lens
import Control.Monad.Combinators.Expr
import Data.Generics.Product.Types
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many)
import Text.Megaparsec

newtype MetaVar = MkVar Int
  deriving stock (Eq, Ord, Show, Generic)

instance Disp MetaVar where
  disp (MkVar i) = "_" <> pretty i

type DiscDiag = [(Label, Typ)]

data TPrim = TInt | TFloat | TString | TBase
  deriving stock (Eq, Show, Generic)

instance Disp TPrim where
  disp p = pretty $ drop 1 (show p :: [Char])

data Typ
  = Lim DiscDiag
  | CoLim DiscDiag
  | TNamed UcIdent
  | TTuple [Typ]
  | TVar MetaVar
  | TPrim TPrim
  | Typ :=> Typ
  | TFunApp LcIdent Typ
  deriving stock (Eq, Show, Generic)

freeVars :: Traversal' Typ MetaVar
freeVars = types @MetaVar

instance Parsed Typ where
  parsed = makeExprParser pAtom operatorTable

operatorTable :: [[Operator Parser Typ]]
operatorTable = [[InfixR ((:=>) <$ symbol "=>")]]

funApp :: Parser Typ
funApp = do
  f <- parsed
  x <- wrapped '(' ')' parsed
  pure (TFunApp f x)

pAtom :: Parser Typ
pAtom =
  lexeme $
    choice
      [ TTuple <$> pTuple (lexeme parsed),
        Lim <$> pBracedFields ':',
        CoLim <$> pBracketedFields ':',
        TNamed <$> parsed,
        funApp
      ]

instance Disp Typ where
  disp = \case
    TFunApp f x -> disp f <> parens (disp x)
    Lim xs -> commaBrace ':' xs
    CoLim xs -> commaBracket ':' xs
    TNamed name -> disp name
    TVar var -> disp var
    TTuple xs -> dispTup xs
    a :=> b -> disp a <+> "=>" <+> disp b
    TPrim p -> disp p
