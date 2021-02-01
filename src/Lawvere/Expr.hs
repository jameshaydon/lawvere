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

data SketchInterp = SketchInterp
  { obs :: [(UcIdent, Expr)],
    ars :: [(LcIdent, Expr)]
  }
  deriving stock (Show)

instance Disp SketchInterp where
  disp SketchInterp {..} =
    braces . vsep . punctuate comma $
      (("ob" <+>) . dispMapping <$> obs)
        ++ (("ar" <+>) . dispMapping <$> ars)
    where
      dispMapping (x, e) = disp x <+> "|->" <+> disp e

instance Parsed SketchInterp where
  parsed = do
    mappings <- pCommaSep '{' '}' pMapping
    let (obs, ars) = partitionEithers mappings
    pure SketchInterp {..}
    where
      pMapping = (Left <$> (kwOb *> pMapsto)) <|> (Right <$> (kwAr *> pMapsto))
      pMapsto :: (Parsed a, Parsed b) => Parser (a, b)
      pMapsto = (,) <$> lexeme parsed <*> (symbol "|->" *> parsed)

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
  | FromFree UcIdent Expr SketchInterp
  | Curry LcIdent Expr
  | Object UcIdent
  | CanonicalInj Expr
  | Abstract Expr
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

pInterp :: Parser Expr
pInterp = do
  kwInterpret
  sketchName <- lexeme parsed
  kwOver
  overThis <- lexeme parsed
  kwWith
  theInterp <- parsed
  pure (FromFree sketchName overThis theInterp)

pCurry :: Parser Expr
pCurry = do
  kwCurry
  lab <- lexeme parsed
  body <- parsed
  pure (Curry lab body)

pAtom :: Parser Expr
pAtom =
  choice
    [ pInterp,
      pCurry,
      try pApp,
      Proj <$> ("." *> parsed),
      try (Inj <$> (parsed <* ".")), -- we need to look ahead for the dot
      Top <$> parsed,
      Lit <$> parsed,
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
      Abstract <$> kwCall kwAbstract,
      Object <$> parsed
    ]

instance Parsed Expr where
  parsed = Comp <$> many (lexeme pAtom)

instance Disp Expr where
  disp = \case
    Object o -> disp o
    Abstract e -> "abstract" <> parens (disp e)
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
    FromFree sketchName overThis interp ->
      vsep
        [ "interpret",
          disp sketchName,
          "over",
          disp overThis,
          "with",
          disp interp
        ]
    Curry lab f -> "curry" <+> disp lab <+> disp f
