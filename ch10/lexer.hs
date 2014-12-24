module Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

lexer :: P.TokenParser ()
lexer = P.makeTokenParser ( L.emptyDef {
                              P.reservedNames = ["if", "then", "else", "true", "false", "Bool"],
                              P.reservedOpNames = ["^", "->", ".", ":"]
                            } )

parens = P.parens lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
