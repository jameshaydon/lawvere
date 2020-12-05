module Lawvere.Decl where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Parse
import Lawvere.Typ
import Prettyprinter
import Protolude hiding (many)
import Text.Megaparsec

data Decl
  = --DOb UcIdent Typ
    DAr LcIdent Typ Expr
  | DMain Expr

type Decls = [Decl]

pDef :: (Parsed name, Parsed body) => Parser () -> Parser a -> Parser (name, a, body)
pDef kw pExtra = do
  kw
  name <- lexeme parsed
  extra <- lexeme pExtra
  _ <- symbol "="
  body <- parsed
  pure (name, extra, body)

-- pObDecl :: Parser Decl
-- pObDecl = do
--   (name, _, body) <- pDef kwOb (pure ())
--   pure (DOb name body)

pArDecl :: Parser Decl
pArDecl = do
  (name, typ, ar) <- pDef kwAr pNameAndNiche
  pure (DAr name typ ar)
  where
    pNameAndNiche = do
      lexChar ':'
      parsed

instance Parsed Decl where
  parsed =
    choice
      [ -- pObDecl,
        pArDecl
      ]

instance Disp Decl where
  disp = \case
    --DOb name ob -> dispDef "ob" (disp name) (disp ob)
    DAr name typ ar -> dispDef "ar" (disp name <+> ":" <+> disp typ) (disp ar)
    DMain ar -> dispDef "main" mempty (disp ar)
    where
      dispDef :: Text -> Doc Ann -> Doc Ann -> Doc Ann
      dispDef defname thing body = nest 2 $ vsep [pretty defname <+> thing <+> "=", body]

instance Parsed Decls where
  parsed = sc *> many (lexeme parsed) <* sc

instance Disp Decls where
  disp ds = vsep ((<> line) . disp <$> ds)
