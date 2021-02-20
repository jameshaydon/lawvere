module Lawvere.Expr where

import Control.Lens
import Control.Monad.Combinators.Expr
import Data.Generics.Labels ()
import Data.List (foldr1)
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Parse
import Lawvere.Scalar
import Prettyprinter
import Protolude hiding (many, try)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

data PrimFn = PrimIdentity | PrimApp | PrimIncr | PrimAbs | PrimShow | PrimConcat
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Disp PrimFn where
  disp = pretty . map toLower . drop 4 . show

data Prim = Pfn PrimFn | PrimOp BinOp
  deriving stock (Eq, Ord, Show)

instance Fin Prim where
  enumerate = (Pfn <$> enumerate) ++ (PrimOp <$> enumerate)

instance Disp Prim where
  disp = \case
    Pfn p -> disp p
    PrimOp o -> case o of
      NumOp no -> case no of
        OpPlus -> "plus"
        OpMinus -> "minus"
        OpTimes -> "mult"
      CompOp co -> case co of
        OpEq -> "equal"
        OpLt -> "less_than"
        OpLte -> "less_than_equal"
        OpGt -> "greater_than"
        OpGte -> "greater_than_equal"

instance Parsed Prim where
  parsed = choice [p <$ try (chunk (render p) >> notFollowedBy (satisfy nonFirstIdentChar)) | p <- enumerate]

data ComponentDecorator = Eff | Pure
  deriving stock (Show, Eq)

data ConeComponent = ConeComponent ComponentDecorator Label
  deriving stock (Show, Eq)

purPos :: Int -> ConeComponent
purPos = ConeComponent Pure . LPos

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
  deriving stock (Show, Eq, Generic)

data NumOp = OpPlus | OpMinus | OpTimes
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Disp NumOp where
  disp = \case
    OpPlus -> "+"
    OpMinus -> "-"
    OpTimes -> "*"

evNumOp :: (Num a) => NumOp -> a -> a -> a
evNumOp OpPlus = (+)
evNumOp OpMinus = (-)
evNumOp OpTimes = (*)

data CompOp = OpEq | OpLt | OpLte | OpGt | OpGte
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Disp CompOp where
  disp = \case
    OpEq -> "=="
    OpLt -> "<"
    OpLte -> "<="
    OpGt -> ">"
    OpGte -> ">="

data BinOp = NumOp NumOp | CompOp CompOp
  deriving stock (Eq, Ord, Show)

instance Fin BinOp where
  enumerate = (NumOp <$> enumerate) ++ (CompOp <$> enumerate)

instance Disp BinOp where
  disp (NumOp o) = disp o
  disp (CompOp o) = disp o

binOp :: (Sca -> p) -> (Bool -> p) -> BinOp -> Sca -> Sca -> p
binOp sca _ (NumOp o) (Int x) (Int y) = sca (Int (evNumOp o x y))
binOp sca _ (NumOp o) (Float x) (Float y) = sca (Float (evNumOp o x y))
binOp _ tf (CompOp o) (Int x) (Int y) = tf $ compa o x y
binOp _ tf (CompOp o) (Float x) (Float y) = tf $ compa o x y
binOp _ _ _ _ _ = panic "bad binop"

compa :: (Ord a) => CompOp -> a -> a -> Bool
compa OpEq = (==)
compa OpLt = (<)
compa OpLte = (<=)
compa OpGt = (>)
compa OpGte = (>=)

data Expr
  = EId
  | BinComp Expr Expr
  | Cone [(ConeComponent, Expr)]
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
  | -- | Curry LcIdent Expr
    Object UcIdent
  | CanonicalInj Expr
  | Side LcIdent Expr
  | SidePrep Label
  | SideUnprep Label
  | BinOp BinOp Expr Expr
  | SumInjLabelVar LcIdent
  | SumUniCoconeVar LcIdent
  deriving stock (Show, Eq)

instance Plated Expr where
  plate _ EId = pure EId
  plate f (BinComp a b) = BinComp <$> f a <*> f b
  plate f (Cone cone) = Cone <$> (each . _2) f cone
  plate f (ELim diag) = ELim <$> (each . _2) f diag
  plate f (Tuple as) = Tuple <$> each f as
  plate f (CoCone cocone) = CoCone <$> (each . _2) f cocone
  plate f (ECoLim diag) = ECoLim <$> (each . _2) f diag
  plate f (InterpolatedString fs) = InterpolatedString <$> (each . #_ISExpr) f fs
  plate _ l@(Lit _) = pure l
  plate _ p@(Proj _) = pure p
  plate _ i@(Inj _) = pure i
  plate f (Comp fs) = Comp <$> each f fs
  plate _ t@(Top _) = pure t
  plate _ d@(Distr _) = pure d
  plate f (EConst e) = f e
  plate _ p@(EPrim _) = pure p
  plate f (EFunApp name e) = EFunApp name <$> f e
  plate _ o@(Object _) = pure o
  plate f (CanonicalInj e) = CanonicalInj <$> f e
  plate f (Side lab e) = Side lab <$> f e
  plate f (BinOp o x y) = BinOp o <$> f x <*> f y
  plate _ lv@(SumInjLabelVar _) = pure lv
  plate _ cv@(SumUniCoconeVar _) = pure cv
  plate _ sp@(SidePrep _) = pure sp
  plate _ su@(SideUnprep _) = pure su

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

-- pCurry :: Parser Expr
-- pCurry = do
--   kwCurry
--   lab <- lexeme parsed
--   Curry lab <$> parsed

pSide :: Parser Expr
pSide = do
  lab <- single '!' *> lexeme parsed
  e <- wrapped '(' ')' parsed
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
  es <- between (symbol "#(") (single ')') (sepBy (lexeme parsed) (lexChar ','))
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

pCanInj :: Parser Expr
pCanInj = CanonicalInj <$> (single '~' *> pAtom)

pAtom :: Parser Expr
pAtom =
  choice
    [ pSide,
      -- pCurry,
      pIfThenElse,
      try pApp,
      pInterpolated,
      EPrim <$> parsed,
      Lit <$> parsed,
      Proj <$> ("." *> parsed),
      try (Inj <$> (parsed <* ".")), -- we need to look ahead for the dot
      Top <$> parsed,
      pCanInj,
      SumUniCoconeVar <$> kwCall kwSumUni,
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
    [] -> EId
    [x] -> x
    [x, y] -> BinComp x y
    _ -> Comp xs

instance Parsed Expr where
  parsed = makeExprParser pComposition operatorTable

instance Disp Expr where
  disp = \case
    EId -> ""
    BinComp f g -> disp f <+> disp g
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
    Cone ps -> commaBrace '=' ps
    ELim ps -> commaBrace ':' ps
    CoCone ps -> commaBracket '=' ps
    ECoLim ps -> commaBracket ':' ps
    Tuple ps -> dispTup ps
    -- Curry lab f -> "curry" <+> disp lab <+> disp f
    Side lab f -> "!" <> disp lab <> braces (disp f)
    InterpolatedString ps -> dquotes (foldMap go ps)
      where
        go (ISRaw t) = pretty t
        go (ISExpr e) = braces (disp e)
    BinOp o x y -> parens (disp x <+> disp o <+> disp y)
    _ -> "TODO"

desugar :: Expr -> Expr
desugar = \case
  Comp [] -> EId
  Comp [x] -> x
  Comp [x, y] -> BinComp x y
  Comp xs -> foldr1 BinComp xs
  Tuple fs -> tupleToCone fs
  BinOp o f g -> binPrim (PrimOp o) f g
  InterpolatedString ps -> foldr go (Lit (Str "")) ps
    where
      go :: ISPart -> Expr -> Expr
      go part e =
        binPrim
          (Pfn PrimConcat)
          ( case part of
              ISRaw t -> Lit (Str t)
              ISExpr f -> f
          )
          e
  e -> e

binPrim :: Prim -> Expr -> Expr -> Expr
binPrim = binApp . EPrim

binApp :: Expr -> Expr -> Expr -> Expr
binApp f x y = Comp [Cone [(purPos 1, x), (purPos 2, y)], f]
