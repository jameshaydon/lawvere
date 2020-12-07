-- |
module Lawvere.Expr where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec

data Expr
  = Cone [(Label, Expr)]
  | Tuple [Expr]
  | CoCone [(Label, Expr)]
  | Lit Sca
  | Proj Label
  | Inj Label
  | Comp [Expr]
  | Top LcIdent
  | Distr Label
  | EConst Expr
  deriving stock (Show)

pAtom :: Parser Expr
pAtom =
  choice
    [ Proj <$> ("." *> parsed),
      try (Inj <$> (parsed <* ".")), -- we need to look ahead for the dot
      Top <$> parsed,
      Lit <$> parsed,
      Tuple <$> pTuple parsed,
      Cone <$> pBracedFields '=',
      CoCone <$> pBracketedFields '=',
      Distr <$> (single '@' *> parsed),
      EConst <$> (chunk "const" *> wrapped '(' ')' parsed)
    ]

instance Parsed Expr where
  parsed = Comp <$> many (lexeme pAtom)

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
