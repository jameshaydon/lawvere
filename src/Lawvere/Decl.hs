module Lawvere.Decl where

import Control.Lens.Plated
import Lawvere.Core
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Ob
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many, some, try)
import Text.Megaparsec

data SketchAr = SketchAr
  { name :: LcIdent,
    source :: Ob,
    target :: Ob
  }

instance Disp SketchAr where
  disp SketchAr {..} =
    disp name <+> ":" <+> disp source <+> "-->" <+> disp target

instance Parsed SketchAr where
  parsed = do
    name <- lexeme parsed
    _ <- symbol ":"
    source <- lexeme parsed
    _ <- symbol "-->"
    target <- parsed
    pure SketchAr {..}

data Sketch = Sketch
  { name :: UcIdent,
    overCat :: UcIdent,
    obs :: [UcIdent],
    ars :: [SketchAr]
  }

instance Disp Sketch where
  disp Sketch {..} =
    dispDef
      "sketch"
      (disp name <+> "over" <+> disp overCat)
      (braces (vsep ((("ob" <+>) . disp <$> obs) ++ (("ar" <+>) . disp <$> ars))))

instance Parsed Sketch where
  parsed = do
    kwSketch
    name <- lexeme parsed
    kwOver
    overCat <- lexeme parsed
    _ <- symbol "="
    (obs, ars) <- partitionEithers <$> pCommaSep '{' '}' (pOb <|> pAr)
    pure Sketch {..}
    where
      pOb = Left <$> (kwOb *> parsed)
      pAr = Right <$> (kwAr *> parsed)

data SketchInterp = SketchInterp
  { obs :: [(UcIdent, Expr)],
    ars :: [(LcIdent, Expr)]
  }
  deriving stock (Show, Generic)

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

data Decl
  = DAr Ob LcIdent (Niche Ob) Expr
  | DOb Ob UcIdent Ob
  | DSketch Sketch
  | DInterp LcIdent UcIdent Expr SketchInterp Expr Expr
  | DCategory Category
  | DEffCat EffCat
  | DEff Effect
  | DEffInterp EffInterp

type Decls = [Decl]

pCat :: Parser Ob
pCat = fromMaybe (OPrim TBase) <$> optional parsed

pObDecl :: Parser Decl
pObDecl = do
  kwOb
  catName <- lexeme parsed
  name <- lexeme parsed
  lexChar '='
  DOb catName name <$> parsed

pArDecl :: Parser () -> (Ob -> LcIdent -> Niche Ob -> Expr -> Decl) -> Parser Decl
pArDecl kw ctor = do
  kw
  catName <- lexeme pCat
  name <- lexeme parsed
  lexChar ':'
  niche <- parsed
  lexChar '='
  ctor catName name niche <$> parsed

pEq :: Parser ()
pEq = lexChar '='

pInterpDecl :: Parser Decl
pInterpDecl = do
  kwInterp
  sketchName <- lexeme parsed
  interpName <- lexeme parsed
  pEq
  kwOver
  overThis <- lexeme parsed
  kwHandling
  theInterp <- lexeme parsed
  kwSumming
  summing <- parsed
  kwSide
  DInterp interpName sketchName overThis theInterp summing <$> parsed

-- For the moment we just support the bare minimum for effect categories
data Category = Category
  { catName :: UcIdent,
    object :: (),
    arr :: Niche Ob -> Niche Ob,
    id :: Expr,
    comp :: (Expr, Expr) -> Expr,
    sumOb :: Maybe (),
    sumInj :: Maybe (Label -> Expr),
    sumUni :: Maybe ([(Label, Expr)] -> Expr)
  }
  deriving stock (Generic)

pNewCat :: Parser Category
pNewCat = do
  kwCat
  catName <- lexeme parsed
  (o, a, i, c, sOb, sIn, sU) <- braced $ do
    _ <- kwOb *> pEq *> kwOb *> symbol "Base"
    pComma
    a <- mkArr <$> (kwAr *> lexeme parsed <* pEq) <*> (kwAr *> parsed <* pColon) <*> parsed
    pComma
    i <- symbol "id" *> pEq *> parsed
    pComma
    c <- do
      (f, g) <- lexeme $ pBuiltin2 "comp"
      pEq
      e <- parsed
      pure (mkComp f g e)
    sOb <- optional $ do
      pComma
      x <- lexeme $ pBuiltin1 "SumOb"
      pEq
      o <- parsed
      pure (mkSumOb x o)
    sIn <- optional $ do
      pComma
      lab <- lexeme $ pBuiltin1 "sumInj"
      pEq
      e <- parsed
      pure (mkSumInj lab e)
    sU <- optional $ do
      pComma
      coconeVar <- lexeme $ pBuiltin1 "sumUni"
      pEq
      e <- parsed
      pure (mkSumUni coconeVar e)
    pure ((), a, i, c, sOb, sIn, sU)
  pure $ Category catName o a i c sOb sIn sU
  where
    mkArr :: Niche UcIdent -> Ob -> Niche Ob -> (Niche Ob -> Niche Ob)
    mkArr (Niche a b) _ (Niche src tgt) (Niche aa bb) = Niche (repl src) (repl tgt)
      where
        repl o = transform go o
        go (ONamed a') | a == a' = aa
        go (ONamed b') | b == b' = bb
        go o = o
    mkComp :: LcIdent -> LcIdent -> Expr -> ((Expr, Expr) -> Expr)
    mkComp f g e (ff, gg) = transform go e
      where
        go (Top f') | f == f' = ff
        go (Top g') | g == g' = gg
        go x = x
    mkSumOb :: LcIdent -> Ob -> ()
    mkSumOb _ _ = ()
    mkSumInj :: LcIdent -> Expr -> (Label -> Expr)
    mkSumInj name e l = transform go e
      where
        go (SumInjLabelVar name') | name == name' = Inj l
        go x = x
    mkSumUni :: LcIdent -> Expr -> [(Label, Expr)] -> Expr
    mkSumUni coconeVar body cocone = transform go body
      where
        go (SumUniCoconeVar coconeVar') | coconeVar == coconeVar' = CoCone cocone
        go x = x

data EffCat = EffCat
  { effCatStructName :: LcIdent,
    effCatName :: UcIdent,
    overCat :: UcIdent,
    canInj :: Expr -> Expr,
    side :: Expr -> Expr
  }
  deriving stock (Generic)

pEffCat :: Parser EffCat
pEffCat = do
  kwEffcat
  effCatStructName <- lexeme parsed
  effCatName <- lexeme parsed
  kwOver
  overCat <- lexeme parsed
  (canInj, side) <- braced $ do
    canInjVar <- lexeme $ pBuiltin1 "i"
    pEq
    canInjExpr <- parsed
    pComma
    sideVar <- lexeme $ pBuiltin1 "side"
    pEq
    sideExpr <- parsed
    pure (mkExprLam canInjVar canInjExpr, mkExprLam sideVar sideExpr)
  pure EffCat {..}
  where
    mkExprLam var body f = transform go body
      where
        go (Top var') | var' == var = f
        go x = x

data Effect = Effect
  { name :: UcIdent,
    overCat :: UcIdent,
    ars :: [(LcIdent, Niche Ob)]
  }

pEff :: Parser Effect
pEff = do
  kwEffect
  name <- lexeme parsed
  kwOver
  overCat <- lexeme parsed
  ars <- pBracedFields ':' Nothing
  pure Effect {..}

data EffInterp = EffInterp
  { effect :: UcIdent,
    interpIn :: UcIdent,
    handlers :: [(LcIdent, Expr)]
  }
  deriving stock (Generic)

pEffInterp :: Parser EffInterp
pEffInterp = do
  kwEffInterp
  effect <- lexeme parsed
  kwIn
  interpIn <- lexeme parsed
  handlers <- pBracedFields '=' Nothing
  pure EffInterp {..}

instance Parsed Decl where
  parsed =
    choice
      [ pObDecl,
        pArDecl kwAr DAr,
        DSketch <$> parsed,
        pInterpDecl,
        DCategory <$> pNewCat,
        DEffCat <$> pEffCat,
        DEff <$> pEff,
        DEffInterp <$> pEffInterp
      ]

instance Disp Decl where
  disp = \case
    DOb catName name ob -> dispDef "ob" (disp catName <+> disp name) (disp ob)
    DAr catName name (Niche a b) body -> dispDef "ar" (disp catName <+> disp name <+> ":" <+> disp a <+> "-->" <+> disp b) (disp body)
    DSketch sketch -> disp sketch
    DInterp {} -> "TODO"
    DCategory {} -> "TODO"
    DEffCat {} -> "TODO"
    DEff {} -> "TODO"
    DEffInterp {} -> "TODO"

dispDef :: Text -> Doc Ann -> Doc Ann -> Doc Ann
dispDef defname thing body = nest 2 $ vsep [pretty defname <+> thing <+> "=", body]

instance Parsed Decls where
  parsed = sc *> some (lexeme parsed) <* sc

instance Disp Decls where
  disp ds = vsep ((<> line) . disp <$> ds)
