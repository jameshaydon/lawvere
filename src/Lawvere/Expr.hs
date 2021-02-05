-- |
module Lawvere.Expr where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec

data Prim = PrimPlus | PrimApp | PrimIncr
  deriving stock (Show)

instance Disp Prim where
  disp =
    ("#prim_" <>) . \case
      PrimPlus -> "plus"
      PrimApp -> "app"
      PrimIncr -> "incr"

data ComponentDecorator = Eff | Pure
  deriving stock (Show)

data ConeComponent = ConeComponent ComponentDecorator Label
  deriving stock (Show)

instance Parsed ConeComponent where
  parsed = do
    eff_ <- optional (single '!')
    ConeComponent (if isJust eff_ then Eff else Pure) <$> parsed

instance Disp ConeComponent where
  disp (ConeComponent Pure lab) = disp lab
  disp (ConeComponent Eff lab) = "!" <> disp lab

componentLabel :: ConeComponent -> Label
componentLabel (ConeComponent _ lab) = lab

data Expr
  = Cone [(ConeComponent, Expr)]
  | ELim [(Label, Expr)]
  | Tuple [Expr]
  | CoCone [(Label, Expr)]
  | ECoLim [(Label, Expr)]
  | Lit Sca
  | Proj Label
  | Inj Label
  | Comp [Expr]
  | Top LcIdent
  | Distr Label
  | EConst Expr
  | EPrim Prim
  | EFunApp LcIdent Expr
  -- | Interp Expr SketchInterp Expr
  | Curry LcIdent Expr
  | Object UcIdent
  | CanonicalInj Expr
  -- | Freyd Expr
  | Side LcIdent Expr
  deriving stock (Show)

-- Tuples are just shorthand for records.
tupleToCone :: [Expr] -> Expr
tupleToCone fs = Cone [(ConeComponent Pure (LPos i), f) | (i, f) <- zip [1 :: Int ..] fs]

kwCall :: Parsed a => Parser () -> Parser a
kwCall kw = kw *> wrapped '(' ')' parsed

pApp :: Parser Expr
pApp = do
  f <- parsed
  e <- wrapped '(' ')' parsed
  pure (EFunApp f e)

{-
pInterp :: Parser Expr
pInterp = do
  -- kwInterpret
  -- sketchName <- lexeme parsed
  kwOver
  overThis <- lexeme parsed
  kwHandling
  theInterp <- lexeme parsed
  kwSumming
  summing <- parsed
  pure (Interp overThis theInterp summing)
-}

pCurry :: Parser Expr
pCurry = do
  kwCurry
  lab <- lexeme parsed
  body <- parsed
  pure (Curry lab body)

pSide :: Parser Expr
pSide = do
  lab <- single '!' *> lexeme parsed
  e <- wrapped '{' '}' parsed
  pure (Side lab e)

pAtom :: Parser Expr
pAtom =
  choice
    [ --pInterp,
      pSide,
      pCurry,
      try pApp,
      Lit <$> parsed,
      Proj <$> ("." *> parsed),
      try (Inj <$> (parsed <* ".")), -- we need to look ahead for the dot
      Top <$> parsed,
      Tuple <$> pTuple parsed,
      -- TODO: try to get rid of the 'try' by committing on the first
      -- label/seperator pair encountered.
      Cone <$> try (pBracedFields '='),
      ELim <$> pBracedFields ':',
      -- TODO: try to get rid of the 'try' by committing on the first
      -- label/seperator pair encountered.
      CoCone <$> try (pBracketedFields '='),
      ECoLim <$> pBracketedFields ':',
      Distr <$> (single '@' *> parsed),
      EConst <$> kwCall kwConst,
      CanonicalInj <$> kwCall kwI,
      --Freyd <$> kwCall kwFreyd,
      Object <$> parsed
    ]

instance Parsed Expr where
  parsed = Comp <$> many (lexeme pAtom)

instance Disp Expr where
  disp = \case
    Object o -> disp o
    CanonicalInj e -> "i" <> parens (disp e)
    EFunApp f e -> disp f <> parens (disp e)
    EPrim p -> disp p
    EConst e -> "const" <> parens (disp e)
    Lit s -> disp s
    Proj p -> "." <> disp p
    Inj i -> disp i <> "."
    Distr l -> "@" <> disp l
    Top t -> disp t
    Comp fs -> align $ sep (disp <$> fs)
    Cone parts -> commaBrace '=' parts
    ELim parts -> commaBrace ':' parts
    CoCone parts -> commaBracket '=' parts
    ECoLim parts -> commaBracket ':' parts
    Tuple parts -> dispTup parts
    Curry lab f -> "curry" <+> disp lab <+> disp f
    Side lab f -> "!" <> disp lab <> braces (disp f)
