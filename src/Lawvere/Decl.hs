module Lawvere.Decl where

import Lawvere.Core
import Lawvere.Disp
import Lawvere.Expr
import Lawvere.Parse
import Lawvere.Typ
import Prettyprinter
import Protolude hiding (many, some)
import Text.Megaparsec

data Decl
  = DAr UcIdent LcIdent Typ Typ Expr
  | DOb UcIdent UcIdent Typ

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
        pArDecl
      ]

instance Disp Decl where
  disp = \case
    DOb catName name ob -> dispDef "ob" (disp catName <+> disp name) (disp ob)
    DAr catName name a b body -> dispDef "ar" (disp catName <+> disp name <+> ":" <+> disp a <+> "-->" <+> disp b) (disp body)
    where
      dispDef :: Text -> Doc Ann -> Doc Ann -> Doc Ann
      dispDef defname thing body = nest 2 $ vsep [pretty defname <+> thing <+> "=", body]

instance Parsed Decls where
  parsed = sc *> some (lexeme parsed) <* sc

instance Disp Decls where
  disp ds = vsep ((<> line) . disp <$> ds)
