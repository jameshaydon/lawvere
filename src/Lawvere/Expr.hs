-- |
module Lawvere.Expr where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (many)
import Text.Megaparsec

data Expr
  = Cone [(LcIdent, Expr)]
  | Tuple [Expr]
  | CoCone [(LcIdent, Expr)]
  | Lit Sca
  | Proj LcIdent
  | Inj LcIdent
  | Comp [Expr]
  | Top LcIdent
  | Distr LcIdent
  | EConst Expr

pAtom :: Parser Expr
pAtom =
  choice
    [ Top <$> parsed,
      Lit <$> parsed,
      Tuple <$> pTuple parsed,
      Proj <$> ("." *> parsed),
      Inj <$> (parsed <* "."),
      Cone <$> pBracedFields '=',
      CoCone <$> pBracketedFields '=',
      Distr <$> (single '@' *> parsed),
      EConst <$> wrapped '(' ')' parsed
    ]

instance Parsed Expr where
  parsed = Comp <$> many pAtom

instance Disp Expr where
  disp = \case
    EConst e -> "const" <> parens (disp e)
    Lit s -> disp s
    Proj p -> "." <> disp p
    Inj i -> disp i <> "."
    Distr l -> "@" <> disp l
    Top t -> disp t
    Comp fs -> align $ sep (disp <$> fs)
    Cone parts -> commaBrace parts
    CoCone parts -> commaBracket parts
    Tuple parts -> dispTup parts
