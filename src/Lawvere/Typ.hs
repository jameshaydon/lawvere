module Lawvere.Typ where

import Control.Monad.Combinators.Expr
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Protolude
import Text.Megaparsec

-- For the moment we are only working on discrete diagrams, so m ~ ().
data Typ
  = Lim [(LcIdent, Typ)]
  | CoLim [(LcIdent, Typ)]
  | TNamed UcIdent
  | TTuple [Typ]
  | TArr Typ Typ

instance Parsed Typ where
  parsed = makeExprParser pAtom operatorTable

operatorTable :: [[Operator Parser Typ]]
operatorTable =
  [[InfixR (TArr <$ symbol "->")]]

pAtom :: Parser Typ
pAtom =
  choice
    [ TTuple <$> pTuple parsed,
      Lim <$> pBracedFields ':',
      CoLim <$> pBracketedFields ':',
      TNamed <$> parsed
    ]

instance Disp Typ where
  disp _ = "TODO"
