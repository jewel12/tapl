module Parser where

import Type
import Lexer

import Control.Applicative hiding ((<|>))
import Data.List
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Combinator as C (chainl1)
    
term :: Context -> Parser Term
term ctx = apply ctx <|> term' ctx
                      
term' :: Context -> Parser Term
term' ctx = try (parens $ term ctx) <|> var ctx  <|> lambda ctx <|> ifTerm ctx

apply :: Context -> Parser Term
apply ctx = C.chainl1 (term' ctx) (TmApp <$ whiteSpace)

lambda :: Context -> Parser Term
lambda ctx = do
  reservedOp "^"
  name <- identifier
  reservedOp ":"
  ty <- typ
  reservedOp "."
  whiteSpace
  t <- term (addName ctx name)
  return (TmAbs name ty t)

typ :: Parser Ty
typ = arrowType <|> typ'
typ' = bool

bool :: Parser Ty
bool = TyBool <$ reserved "Bool"
       
arrowType :: Parser Ty
arrowType = C.chainl1 typ' ( TyArr <$ arrow )
    where arrow = reservedOp "->"

var :: Context -> Parser Term
var ctx = true <|> false <|> namedVar ctx

true :: Parser Term
true = TmTrue <$ reserved "true"

false :: Parser Term
false = TmFalse <$ reserved "false"

namedVar ::  Context -> Parser Term
namedVar ctx = do
  name <- identifier
  idx <- case name2index ctx name of
           Just idx -> return idx
           Nothing -> fail $ name ++ " is not defined."
  return (TmVar idx (fromIntegral (length ctx)))

ifTerm ::  Context -> Parser Term
ifTerm ctx = do
  reserved "if"
  whiteSpace
  t1 <- term ctx
  whiteSpace
  reserved "then"
  whiteSpace
  t2 <- term ctx
  whiteSpace
  reserved "else"
  whiteSpace
  t3 <- term ctx
  return (TmIf t1 t2 t3)

name2index :: Context -> Name -> Maybe Integer
name2index ctx name = fromIntegral <$> findIndex (\x -> name == (fst x)) ctx

addName :: Context -> Name -> Context
addName ctx name = (name, NameBind) : ctx

runParser :: String -> Either String Term
runParser str = case parse (term []) "parser error" str of
                  Left e -> fail $ show e
                  Right term -> return term
