-- |
module Lawvere.Expr where

import Control.Monad.Combinators.Expr
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

data Prim = PrimPlus | PrimApp | PrimIncr | PrimAbs | PrimShow
  deriving stock (Show)

instance Disp Prim where
  disp =
    ("#prim_" <>) . \case
      PrimPlus -> "plus"
      PrimApp -> "app"
      PrimIncr -> "incr"
      PrimAbs -> "abs"
      PrimShow -> "show"

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

data ISPart = ISRaw Text | ISExpr Expr
  deriving stock (Show)

data NumOp = OpPlus | OpMinus | OpTimes
  deriving stock (Show)

instance Disp NumOp where
  disp = \case
    OpPlus -> "+"
    OpMinus -> "-"
    OpTimes -> "*"

data CompOp = OpEq | OpLt | OpLte | OpGt | OpGte
  deriving stock (Show)

instance Disp CompOp where
  disp = \case
    OpEq -> "=="
    OpLt -> "<"
    OpLte -> "<="
    OpGt -> ">"
    OpGte -> ">="

data BinOp = NumOp NumOp | CompOp CompOp
  deriving stock (Show)

instance Disp BinOp where
  disp (NumOp o) = disp o
  disp (CompOp o) = disp o

data Expr
  = Cone [(ConeComponent, Expr)]
  | ELim [(Label, Expr)]
  | Tuple [Expr]
  | CoCone [(Label, Expr)]
  | ECoLim [(Label, Expr)]
  | InterpolatedString [ISPart]
  | Lit Sca
  | Proj Label
  | Inj Label
  | Comp [Expr]
  | Top LcIdent
  | Distr Label
  | EConst Expr
  | EPrim Prim
  | EFunApp LcIdent Expr
  | Curry LcIdent Expr
  | Object UcIdent
  | CanonicalInj Expr
  | Side LcIdent Expr
  | BinOp BinOp Expr Expr
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

pInterpolated :: Parser Expr
pInterpolated = Char.char '"' *> (InterpolatedString <$> manyTill (pE <|> try pRaw) (Char.char '"'))
  where
    pRaw = ISRaw . toS <$> escapedString
    pE = ISExpr <$> (Char.char '{' *> parsed <* Char.char '}')
    escapedString = catMaybes <$> someTill ch (lookAhead (Char.char '"' <|> Char.char '{'))
    ch =
      choice
        [ Just <$> L.charLiteral,
          Nothing <$ Char.string "\\&",
          Just '{' <$ Char.string "\\{",
          Just '}' <$ Char.string "\\}"
        ]

pTupledOrParensed :: Parser Expr
pTupledOrParensed = do
  xs <- pTuple parsed
  pure $ case xs of
    [x] -> x
    _ -> Tuple xs

pList :: Parser Expr
pList = do
  es <- between (chunk "#(") (single ')') (sepBy (lexeme parsed) (lexChar ','))
  pure $
    foldr
      ( \hd tl ->
          Comp
            [ Cone
                [ (ConeComponent Pure (LNam "head"), hd),
                  (ConeComponent Pure (LNam "tail"), tl)
                ],
              Inj (LNam "cons")
            ]
      )
      (Inj (LNam "empty"))
      es

pIfThenElse :: Parser Expr
pIfThenElse = do
  kwIf
  cond <- parsed
  kwThen
  tt <- parsed
  kwElse
  ff <- parsed
  pure $
    Comp
      [ Cone
          [ (ConeComponent Pure (LNam "v"), Comp []),
            (ConeComponent Pure (LNam "case"), cond)
          ],
        Distr (LNam "case"),
        CoCone
          [ (LNam "true", Comp [Proj (LNam "v"), tt]),
            (LNam "false", Comp [Proj (LNam "v"), ff])
          ]
      ]

pAtom :: Parser Expr
pAtom =
  choice
    [ pSide,
      pCurry,
      pIfThenElse,
      try pApp,
      pInterpolated,
      Lit <$> parsed,
      Proj <$> ("." *> parsed),
      try (Inj <$> (parsed <* ".")), -- we need to look ahead for the dot
      Top <$> parsed,
      CanonicalInj <$> kwCall kwI,
      pList,
      pTupledOrParensed,
      -- TODO: try to get rid of the 'try' by committing on the first
      -- label/seperator pair encountered.
      Cone <$> try (pBracedFields '=' conePunner),
      ELim <$> pBracedFields ':' Nothing,
      -- TODO: try to get rid of the 'try' by committing on the first
      -- label/seperator pair encountered.
      CoCone <$> try (pBracketedFields '=' coconePunner),
      ECoLim <$> pBracketedFields ':' Nothing,
      Distr <$> (single '@' *> parsed),
      EConst <$> kwCall kwConst,
      Object <$> parsed
    ]
  where
    conePunner :: Maybe (ConeComponent -> Expr)
    conePunner = Just $ \case
      ConeComponent Pure lab -> Proj lab
      ConeComponent Eff lab -> CanonicalInj (Proj lab)
    coconePunner = Just Inj

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [numOp OpTimes "*"],
    [numOp OpMinus "-", numOp OpPlus "+"],
    [compOp OpEq "==", compOp OpLte "<=", compOp OpLt "<", compOp OpGte ">=", compOp OpGt ">"]
  ]
  where
    infixR o t = InfixR (BinOp o <$ symbol t)
    numOp = infixR . NumOp
    compOp = infixR . CompOp

pComposition :: Parser Expr
pComposition = do
  xs <- many (lexeme pAtom)
  pure $ case xs of
    [] -> Comp []
    [x] -> x
    _ -> Comp xs

instance Parsed Expr where
  parsed = makeExprParser pComposition operatorTable

instance Disp Expr where
  disp = \case
    Object o -> disp o
    CanonicalInj e -> angles (disp e)
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
    InterpolatedString ps -> dquotes (foldMap go ps)
      where
        go (ISRaw t) = pretty t
        go (ISExpr e) = braces (disp e)
    BinOp op x y -> parens (disp x <+> disp op <+> disp y)
