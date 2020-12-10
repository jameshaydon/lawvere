module Lawvere.Decl where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Ob
import Lawvere.Parse
import Prettyprinter
import Protolude hiding (many, some)
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

data Decl
  = DAr Ob LcIdent Ob Ob Expr
  | DOb Ob UcIdent Ob
  | DSketch Sketch

type Decls = [Decl]

pObDecl :: Parser Decl
pObDecl = do
  kwOb
  catName <- lexeme parsed
  name <- lexeme parsed
  lexChar '='
  ob <- parsed
  pure (DOb catName name ob)

pArDecl :: Parser Decl
pArDecl = do
  kwAr
  catName <- lexeme parsed
  name <- lexeme parsed
  lexChar ':'
  a <- lexeme parsed
  _ <- symbol "-->"
  b <- lexeme parsed
  lexChar '='
  body <- parsed
  pure (DAr catName name a b body)

instance Parsed Decl where
  parsed =
    choice
      [ pObDecl,
        pArDecl,
        DSketch <$> parsed
      ]

instance Disp Decl where
  disp = \case
    DOb catName name ob -> dispDef "ob" (disp catName <+> disp name) (disp ob)
    DAr catName name a b body -> dispDef "ar" (disp catName <+> disp name <+> ":" <+> disp a <+> "-->" <+> disp b) (disp body)
    DSketch sketch -> disp sketch

dispDef :: Text -> Doc Ann -> Doc Ann -> Doc Ann
dispDef defname thing body = nest 2 $ vsep [pretty defname <+> thing <+> "=", body]

instance Parsed Decls where
  parsed = sc *> some (lexeme parsed) <* sc

instance Disp Decls where
  disp ds = vsep ((<> line) . disp <$> ds)
