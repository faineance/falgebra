module Parser where
import Protolude
import Text.Megaparsec.Char
import Text.Megaparsec

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme        = L.lexeme space
integer       = lexeme L.decimal

